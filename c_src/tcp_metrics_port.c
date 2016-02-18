#include <fcntl.h>
#include <stdbool.h>
#include <unistd.h>

#include <sys/socket.h>
#include <netinet/ip.h>

#include <linux/genetlink.h>
#include <linux/tcp_metrics.h>

#include <netlink/netlink.h>
#include <netlink/socket.h>
#include <netlink/msg.h>
#include <netlink/genl/ctrl.h>
#include <netlink/genl/genl.h>


static void update(uint32_t ip, uint32_t rtt)
{
    fwrite(&ip, sizeof(ip), 1, stdout);
    fwrite(&rtt, sizeof(rtt), 1, stdout);
}


static uint64_t age_threshold_ms = 0;

static int handle_packet(struct nl_msg *msg, void *arg)
{
    struct nlmsghdr *hdr = nlmsg_hdr(msg);
    if (!hdr)
        return NL_SKIP;
    struct nlattr *attrs[1+TCP_METRICS_ATTR_MAX];
    int rv = genlmsg_parse(hdr, 0, attrs, TCP_METRICS_ATTR_MAX, NULL);
    if (rv)
        return NL_SKIP;

    /* Right now, we only care about IPv4 addresses with RTT info. */
    if (!attrs[TCP_METRICS_ATTR_ADDR_IPV4] || !attrs[TCP_METRICS_ATTR_VALS])
        return NL_SKIP;

    struct nlattr *p = attrs[TCP_METRICS_ATTR_ADDR_IPV4];

    static uint32_t last_addr = 0;
    uint32_t addr;
    if (nla_len(p) != sizeof(addr))
        return NL_SKIP;
    addr = nla_get_u32(p);
    /* If we already have a value for this address, we don't need
     * more.
     *
     * THIS IS A HACK: we don't have any actual guarantees about
     * order, but this is a cheap way to avoid dups, since at least
     * the current (4.3.x) implementation sends all the entries with
     * the same address in ascending order of age. */
    if (addr == last_addr)
        return NL_SKIP;
    last_addr = addr;

    p = attrs[TCP_METRICS_ATTR_AGE];
    uint64_t age = 0;
    /* actually millisecs, not microsecs, despite many levels of
     * confusing names. (see libnl documentation to be further
     * confused.) */
    if (p)
        age = nla_get_msecs(p);

    if (age_threshold_ms && age >= age_threshold_ms)
        return NL_SKIP;

    p = attrs[TCP_METRICS_ATTR_VALS];
    /* XXX tcp_metrics returns one extra metric, and I have no idea
     * why. */
    struct nlattr *metrics[2+TCP_METRIC_MAX];
    rv = nla_parse_nested(metrics, 1+TCP_METRIC_MAX, p, NULL);
    if (rv)
        return NL_SKIP;

    /* XXX It seems that rtt is encoded as 61.3 fixed point, and
     * variance as 62.2.  I have been unable to find documentation
     * that explains why. */
    unsigned long rtt = 0;
    struct nlattr *m;
    if ((m = metrics[1+TCP_METRIC_RTT]))
        rtt = nla_get_u32(m);
    else
        return NL_SKIP;

    update(addr, rtt>>3);

    return NL_OK;
}


static void print_usage(const char *name)
{
    fprintf(stderr, "Usage:\n%s [interval]\n\n"
            "Note that interval is in whole seconds, and must be at least 1.\n",
            name);
}


int main(int argc, char **argv)
{
    int interval = 5;

    if (argc > 2) {
        print_usage(argv[0]);
        return 1;
    } else if (argc == 2)
        interval = atoi(argv[1]);

    if (interval < 1) {
        print_usage(argv[0]);
        return 1;
    }

    struct nl_sock *sk = nl_socket_alloc();
    if (!sk)
        return 1;

    int rv = genl_connect(sk);
    if (rv)
        return rv;

    int family = genl_ctrl_resolve(sk, TCP_METRICS_GENL_NAME);
    if (family < 0)
        return 1;

    rv = nl_socket_modify_cb(sk, NL_CB_VALID, NL_CB_CUSTOM, handle_packet, NULL);
    if (rv)
        return rv;

    struct nl_msg *msg = nlmsg_alloc();
    if (!msg)
        return 1;

    /* Dump everything the first time. */
    age_threshold_ms = 0;

    /* Erlang hooks stdin up to a tty, so don't try isatty() here.  We
     * try non-blocking reads to see if Erlang has closed our stdin
     * yet. */
    int fd = 0;
    if (fcntl(fd, F_SETFL, O_NONBLOCK | fcntl(fd, F_GETFL, 0)))
        return 1;
    while ((rv = read(fd, (char[1]){0}, 1)) < 0) {
        int flags = NLM_F_DUMP;
        uint8_t cmd = TCP_METRICS_CMD_GET;
        genlmsg_put(msg, NL_AUTO_PORT, NL_AUTO_SEQ, family, 0,
                    flags, cmd, TCP_METRICS_GENL_VERSION);

        rv = nl_send_auto(sk, msg);
        if (rv < 0)
            return 1;

        rv = nl_recvmsgs_default(sk);
        if (rv)
            return rv;

        fflush(stdout);

        sleep(interval);
        age_threshold_ms = (1+interval) * 1000;
    }
    return rv;
}

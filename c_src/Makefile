
.PHONY: clean

STAGING := ../priv
CFLAGS := -Wall -g

$(STAGING)/tcp_metrics_port: tcp_metrics_port.c | $(STAGING)
	$(CC) $(CFLAGS) -o $@ $^ $(shell pkg-config --cflags --libs libnl-3.0 libnl-genl-3.0)

$(STAGING):
	mkdir -p $@

clean:
	$(RM) ../priv/tcp_metrics_port

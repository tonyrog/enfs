# Some modules are automatically generated, so we won't use the
# include makefile's auto-detected list.
MODULES := nfs_svc nfs_xdr nfs_server nfs_procfs nfs_filefs
ERLC = erlc
EBIN = ../ebin
OBJECTS := $(MODULES:%=$(EBIN)/%.beam)
ERLC_FLAGS = -Wall

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS) nfs_svc.erl nfs_xdr.erl nfs.hrl

nfs.hrl: nfs.x
	erpcgen -hrl nfs.x

nfs_svc.erl: nfs.x
	erpcgen -svc nfs.x

nfs_xdr.erl: nfs.x
	erpcgen -xdr nfs.x

$(EBIN)/nfs_server.beam: nfs.hrl
$(EBIN)/nfs_svc.beam: nfs.hrl

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

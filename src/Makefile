# Some modules are automatically generated, so we won't use the
# include makefile's auto-detected list.
MODULES := \
	enfs_app \
	enfs_sup \
	nfs_server \
	nfs_svc \
	nfs_xdr \
	nfs_procfs \
	nfs_filefs \
	nfs_sshfs

comma := ,
empty := 
space := $(empty) $(empty)
COMMA_MODULES := $(subst $(space),$(comma),$(MODULES))


ERLC = erlc
EBIN = ../ebin
OBJECTS := $(MODULES:%=$(EBIN)/%.beam)
ERLC_FLAGS = -Wall

all: $(OBJECTS) $(EBIN)/enfs.app

clean:
	rm -f $(OBJECTS) nfs_svc.erl nfs_xdr.erl nfs.hrl

nfs.hrl: nfs.x
	erpcgen -hrl nfs.x

nfs_svc.erl: nfs.x
	erpcgen -svc nfs.x

nfs_xdr.erl: nfs.x
	erpcgen -xdr nfs.x

$(EBIN)/enfs.app: enfs.app.src
	sed \
	-e "s/[{]vsn, git[}]/{vsn, \"`git describe --always --tags`\"}/g" \
	-e "s/MODULES/$(COMMA_MODULES)/g" enfs.app.src > $@


$(EBIN)/nfs_server.beam: nfs.hrl
$(EBIN)/nfs_svc.beam: nfs.hrl

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

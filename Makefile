
time=$(shell /bin/date +%F-%H-%M)
bundle=logiku-$(time).git
encrypted_bundle=$(bundle).gpg
password="nK\"]q4wA!lx;y:"
current_dir = $(shell pwd)


all : $(encrypted_bundle)

$(encrypted_bundle) : $(bundle)
	gpg-encrypt.py $^  $(password)

$(bundle) :
	git bundle create $(bundle) --all

.PHONY: $(bundle)

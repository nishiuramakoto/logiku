# Need to set appropriate policy via IAM console

EC2=$(shell ec2-host)

current_dir = $(shell pwd)
time=$(shell /bin/date +%F-%H-%M)
#time=$(shell /bin/date +%F-%H)

#bundle=bundles/logiku-$(time).git
#subbundle=bundles/prolog-fd-$(time).git
bundle=bundles/logiku.git
subbundle=bundles/prolog-fd.git
password_test_gpg= test/password-test.gpg

encrypted_bundles= $(bundle).gpg $(subbundle).gpg

#apg -a 1 -M sncl -m 14 -x 14

password=$(shell cat gpg-password)

bootstrap:
	stack setup && stack build && stack exec logiku

backup : challenge-password  cp-to-s3

%.git.gpg : %.git
	gpg  -c --passphrase $(password)  $^
#	gpg-encrypt.py $^  $(password)

$(bundle) :
	git bundle create $(bundle) --all

$(subbundle) :
	git bundle create $(subbundle) --all

challenge-password : test/password-test.gpg
	@rm test/password-test > /dev/null 2>&1 ;\
	 gpg  $^

cp-to-s3 : $(encrypted_bundles)
	for x in $(encrypted_bundles) ;do \
		aws s3 cp  $$x  s3://rasm-backup-northern-california ;\
	done

logiku.keter :
	yesod keter

install-keter: logiku.keter
	cp logiku.keter ec2/opt/keter/incoming/

rsync : install-keter
	rsync -avz --exclude '**/temp' --exclude '**/log' \
		-e 'ssh -i /mnt/crypt/home/makoto/private/ec2-keypair.pem' ec2/ ubuntu@$(EC2):ec2

run-keter : install-keter
	sudo ec2/opt/keter/bin/keter  ec2/opt/keter/etc/keter-config.yaml


change-ip:
	git submodule sync

run:
	echo running logiku ...
	stack exec logiku

test-all :
	(cd prolog-fd && cabal test) ; \
	cabal test
test:
	cabal test

.PHONY: $(bundle) $(subbundle) challenge-password cp-to-s3 change-ip run test

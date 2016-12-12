deps:
	rm -rf .cabal-sandbox/
	cabal sandbox init
	cabal update
	cabal install --only-dependencies
	cabal configure
build: deps
	cabal build
docker-image:
	docker build -t hayoo-build $(shell pwd)
ci: docker-image
	# docker run -v $(shell pwd):/hayoo-cli -ti hayoo-build bash
	docker run -v $(shell pwd):/hayoo-cli -t hayoo-build bash -c "cd /hayoo-cli && make build"

APP_NAME = advent
CL       = sbcl
CL_OPTS  = --noinform --no-sysinit --no-userinit
LISP_SRC = $(wildcard src/*lisp) \
             advent.asd
BUILDDIR = build
LIBS     = $(BUILDDIR)/libs.stamp
QLDIR    = $(BUILDDIR)/quicklisp
MANIFEST = $(BUILDDIR)/manifest.ql
APP      = $(BUILDDIR)/bin/$(APP_NAME)
LOG      = $(BUILDDIR)/build.log
BUILDAPP = $(BUILDDIR)/bin/buildapp.sbcl$
CL_OPTS  = --noinform --no-sysinit --no-userinit

ifeq ($(COMPRESS_CORE),true)
	COMPRESS = --compress-core
else
	COMPRESS =
endif

all: $(APP) ;

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(BUILDDIR)/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(BUILDDIR)/quicklisp.lisp \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
             --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;


$(LIBS): $(QLDIR)/setup.lisp
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp \
		--load deps.lisp \
		--eval '(quit)'
	touch $@

libs: $(LIBS) ;

cleanlibs:
	rm -rf $(QLDIR)/setup.lisp


$(MANIFEST): $(LIBS)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp \
             --eval '(ql:write-asdf-manifest-file "$(MANIFEST)")' \
             --eval '(quit)'

qlmanifest: $(MANIFEST) ;


$(BUILDAPP): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp \
        --eval '(ql:quickload "buildapp")' \
        --eval '(buildapp:build-buildapp "$@")' \
        --eval '(quit)'

buildapp: $(BUILDAPP) ;


$(APP): $(MANIFEST) $(BUILDAPP) $(LISP_SRC)
	mkdir -p $(BUILDDIR)/bin
	$(BUILDAPP) \
		--logfile $(LOG) \
        $(BUILDAPP_OPTS) \
        --sbcl $(CL) \
        --asdf-path . \
        --asdf-tree $(QLDIR)/local-projects \
        --manifest-file $(MANIFEST) \
        --asdf-tree $(QLDIR)/dists \
        --asdf-path . \
        --load-system $(APP_NAME) \
        --entry $(APP_NAME):main \
		$(COMPRESS) \
        --output $@


cleanapp:
	rm -rf $(APP)

run: $(APP)
	./$(APP)

clean:
	rm -rf $(LIBS) $(QLDIR) $(MANIFEST) $(BUILDAPP) $(APP)

test: $(APP)
	sbcl \
		--load $(QLDIR)/setup.lisp \
		--eval '(ql:quickload :advent)' \
		--eval '(ql:quickload :advent/tests)' \
		--eval '(ql:quickload :fiveam)' \
		--eval '(asdf:test-system :advent)' \
		--eval '(quit)'


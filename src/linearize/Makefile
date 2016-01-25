######################################################################
##								    ##
## A standard Makefile that should pretty much work for all local   ##
## projects including those with hierarchical directory structure.  ##
##								    ##
## For your average project it should only be necessary to add      ##    
## SOURCES and TARGETS. 					    ##
## You can find out dependencies with make .depend                  ##
## They are currently not automatically included into the Makefile  ##
##								    ##
######################################################################

######################################################################
## SDAG specific stuff
## Dealing with special environment variables
######################################################################


OCAML_DIR=$(SDAG_OCAML_DIR)
DOC_PREFIX=$(SDAG_DOC_PREFIX)
PRE_LATEX_DOCDIR=$(SDAG_LATEX_DOCDIR)
PRE_HTML_DOCDIR=$(SDAG_HTML_DOCDIR)


LATEX_GEN=odoc_sdag_latex.cmo
HTML_GEN=odoc_sdag_html.cmo

## No more SDAG beyond this point!
######################################################################

######################################################################
##
##  Here come the actual changes to customise the Makefile 
##  for your particular project.
##
######################################################################
# All Source Files, ordered with respect to their dependencies:

SOURCES= jsonfio.ml \
	 synt.ml \
	 util.ml \
	 preprocessor.ml \
	 linearize.ml

# All Target Files, ordered with respect to their priority:

TARGETS=linearizer.ml

# All Packages needed for making sources and targets

PACKAGES=json-wheel

# A flag specifying if documentation is in short or long format
SHORTDOC=false

# The following packages are not always necessary. 
# If you are sure you don't need them, comment them out and 
# you will get slightly smaller binaries. 
PACKAGES+=unix str# Leave that comment to avoid unwanted space!!! 

######################################################################
###
### No more changes beyond this point. 
### If you need to change anything, please notify V.Sorge@cs.bham.ac.uk
###

## Standard Executables
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLFIND=ocamlfind
OCAMLFC=$(OCAMLFIND) $(OCAMLC)
OCAMLFOPT=$(OCAMLFIND) $(OCAMLOPT)

## Assigning Special Directories

SRCDIR=./
LIBDIR=$(OCAML_DIR)/lib
DOCDIR=$(OCAML_DIR)/doc
BINDIR=$(OCAML_DIR)/bin

SOURCES:=$(addprefix $(SRCDIR),$(SOURCES))
TARGETS:=$(addprefix $(SRCDIR),$(TARGETS))

ifneq ($(PRE_LATEX_DOCDIR),)
	LATEX_DOCDIR=$(PRE_LATEX_DOCDIR)
	LATEX_DOC_RM_COMMAND=rm -f $(LATEX_DOCDIR)/*
else
	ifneq ($(DOC_PREFIX),)
		LATEX_DOCDIR=$(DOC_PREFIX)/latex
	else
		LATEX_DOCDIR=latex
	endif
	LATEX_DOC_RM_COMMAND=rm -rf $(LATEX_DOCDIR)
endif

ifneq ($(PRE_HTML_DOCDIR),)
	HTML_DOCDIR=$(PRE_HTML_DOCDIR)
	HTML_DOC_RM_COMMAND=rm -f $(HTML_DOCDIR)/*
else
	ifneq ($(DOC_PREFIX),)
		HTML_DOCDIR=$(DOC_PREFIX)/html
	else
		HTML_DOCDIR=html
	endif
	HTML_DOC_RM_COMMAND=rm -rf $(HTML_DOCDIR)
endif

## The stuff from here on should stay pretty much the same for every project,
## unless we are using subdirectories. That should be tested in more detail.
## 

## Main objects for byte code
MAIN_OBJS= $(foreach source,$(SOURCES),$(addsuffix .cmi,$(basename $(filter %.mli,$(source))))$(addsuffix .cmo,$(basename $(filter %.ml %.mll %.mly,$(source)))))
## Main objects for native code
MAIN_OBJS_OPT=$(foreach source,$(SOURCES),$(addsuffix .cmi,$(basename $(filter %.mli,$(source))))$(addsuffix .cmx,$(basename $(filter %.ml %.mll %.mly,$(source)))))


## This lets us handle hierarchical subdirectory structures if the code is not flat.
## Needs some testing though...
INCLUDEDIRS=$(sort $(dir $(SOURCES) $(TARGETS)))
INCLUDES=$(addprefix -I ,$(INCLUDEDIRS)) $(addprefix -I ,$(LIBDIR))

DOCINCLUDEDIRS=$(sort $(foreach package,$(PACKAGES),$(shell export OCAMLPATH=$(LIBDIR);$(OCAMLFIND) query $(package))))
DOCINCLUDES=$(addprefix -I , $(DOCINCLUDEDIRS)) 


## Flags
# PREPROCESS=-pp 'camlp4o -I . $(addsuffix .cmo,$(basename $(EXTENSIONS)))' # Preprocessing not used

comma:= ,
empty:=
space:= $(empty) $(empty)

OCAMLFINDFLAGS=-package $(subst $(space),$(comma),$(PACKAGES))

OCAMLFLAGS=$(OCAMLFINDFLAGS) $(INCLUDES) # -custom unix.cma str.cma # $(PREPROCESS)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(OCAMLFINDFLAGS) $(INCLUDES) # -ccopt -static #unix.cmxa str.cmxa # $(PREPROCESS) # add other options for ocamlopt here

## Docu Flags for our special generators
ifeq ($(SHORTDOC),true)
	SHORTFLAG = -short
endif

OCAMLHTMLDOCFLAGS=$(DOCINCLUDES) $(INCLUDES) $(SHORTFLAG) -i $(DOCDIR) -g $(HTML_GEN) -d $(HTML_DOCDIR)  -hide Pervasives
OCAMLLATEXDOCFLAGS=$(DOCINCLUDES) $(INCLUDES) $(SHORTFLAG) -i $(DOCDIR) -g $(LATEX_GEN) -o $(LATEX_DOCDIR)/ocamldoc.tex -sepfiles -hide Pervasives

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .cma .cmxa .o .a

.ml.cmo:
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) $(OCAMLOPTFLAGS) -c $<

.mll.cmo:
	$(OCAMLLEX) $<
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) $(OCAMLFLAGS) -c $(addsuffix .ml,$(basename $<))

.mll.cmx:
	$(OCAMLLEX) $<
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) $(OCAMLOPTFLAGS) -c $(addsuffix .ml,$(basename $<))

.mly.cmo: $(addsuffix .cmo,$(basename $(filter %.mll,$(SOURCES))))
	$(OCAMLYACC) $<
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) $(OCAMLFLAGS) -c $(addsuffix .mli,$(basename $<))
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) $(OCAMLFLAGS) -c $(addsuffix .ml,$(basename $<))

.mly.cmx: $(addsuffix .cmo,$(basename $(filter %.mll,$(SOURCES))))
	$(OCAMLYACC) $<
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) $(OCAMLOPTFLAGS) -c $(addsuffix .mli,$(basename $<))
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) $(OCAMLOPTFLAGS) -c $(addsuffix .ml,$(basename $<))
.mly.cmi: $(addsuffix .cmo,$(basename $<))

.cmo.cma:
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) -a -o $(addsuffix .cma,$(basename $<)) $<

.cmx.cmxa:
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) -a -o $(addsuffix .cmxa,$(basename $<)) $<

.o.a:
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) -a -o $(addsuffix .a,$(basename $<)) $<


# Build

## Byte Code
$(basename $(notdir $(TARGETS))): $(MAIN_OBJS) $(addsuffix .cmo,$(basename $(filter %.ml,$(TARGETS))))
	@echo Linking $@
	export OCAMLPATH=$(LIBDIR); $(OCAMLFC) -o $@ $(OCAMLFLAGS) $(MAIN_OBJS) $(addsuffix .cmo,$@) -linkpkg

## Native Code
$(addsuffix .opt,$(basename $(notdir $(TARGETS)))): $(MAIN_OBJS_OPT) $(addsuffix .cmx,$(basename $(filter %$(subst .opt,,$@).ml,$(TARGETS))))
	@echo Linking $@
	export OCAMLPATH=$(LIBDIR); $(OCAMLFOPT) -o $@ $(OCAMLOPTFLAGS) $(MAIN_OBJS_OPT) $(addsuffix .cmx,$(basename $@)) -linkpkg


# Making the Main Targets

.PHONY: all opt
all:
	make clean_byte 
	make all_targets

opt: 
	make clean_native 
	make opt_targets

all_targets: $(basename $(notdir $(TARGETS)))

opt_targets: $(addsuffix .opt, $(basename $(notdir $(TARGETS))))


# Just making sources for libraries

.PHONY: lib opt_lib
lib: $(MAIN_OBJS) $(addsuffix .cmo,$(basename $(filter %.ml,$(TARGETS)))) $(addsuffix .cma,$(basename $(filter %.ml,$(SOURCES) $(TARGETS))))

opt_lib: $(MAIN_OBJS_OPT) $(addsuffix .cmx,$(basename $(filter %$(subst .opt,,$@).ml,$(TARGETS)))) $(addsuffix .cmxa,$(basename $(filter %.ml,$(SOURCES) $(TARGETS))))

# Installing binaries and libraries

.PHONY: install opt_install install_lib uninstall uninstall_lib
install: all_targets
	@mkdir -p $(BINDIR)
	@for i in $(basename $(TARGETS)); do \
		echo Installing $$i ; \
		(cp $$i $(BINDIR)) \
	done

opt_install: opt_targets
	@mkdir -p $(BINDIR)
	@for i in $(addsuffix .opt, $(basename $(TARGETS))); do \
		echo Installing $$i ; \
		(cp $$i $(BINDIR)) \
	done

##install_lib: lib opt_lib $(addsuffix ._lib, $(INCLUDEDIRS))
install_lib: lib opt_lib $(addprefix $(LIBDIR)/,$(filter-out ./,$(INCLUDEDIRS))) lib_local

## install_sub_lib
$(addprefix  $(LIBDIR)/,$(filter-out ./,$(INCLUDEDIRS))): $(addsuffix META,$(filter-out ./,$(INCLUDEDIRS)))
	@echo $(if $(wildcard $(addsuffix META,$@)),\
		$(shell echo "Library $(notdir $(subst /META,,$(addsuffix META,$@))) already installed! Uninstall old library first."),\
		$(shell $(OCAMLFIND) install -destdir $(LIBDIR) $(notdir $@) $(notdir $@)/META $(notdir $@)/*.cm[ioax] $(notdir $@)/*.cmxa $(notdir $@)/*.[oa] $(notdir $@)/*.ml*))

## install_single_lib
lib_local: META $(addprefix $(LIBDIR)/,$(notdir $(sort $(basename $(wildcard ./*.cma ./*.cmax ./*.a)))))

$(addprefix $(LIBDIR)/,$(notdir $(sort $(basename $(wildcard ./*.cma ./*.cmax ./*.a))))):
	@for i in $(notdir $(sort $(basename $(wildcard ./*.cma ./*.cmax ./*.a)))); do \
		echo Installing $$i ; \
		($(OCAMLFIND) install -destdir $(LIBDIR) $$i META $$i.cm* $$i.[oa] $$i.ml*) \
	done

## building the META files (if necessary).
## Observe that META files will NOT be removed by uninstall or clean!
$(addsuffix META,$(filter-out ./,$(INCLUDEDIRS))):
	@echo Making $@
	@echo 'description = "'$(subst /META,,$@)' Library"' > $@
	@echo 'version = "[Unknown]"' >> $@
	@echo 'archive(byte) = "'$(notdir $(wildcard $(dir $@)*.cma))'"' >> $@
	@echo 'archive(native) = "'$(notdir $(wildcard $(dir $@)*.cmxa))'"' >> $@


META: 
	@echo 'description = "'$(basename $(notdir $(wildcard ./*.cma)))' Library"' > ./META
	@echo 'version = "[Unknown]"' >> ./META
	@echo 'archive(byte) = "'$(notdir $(wildcard ./*.cma))'"' >> ./META
	@echo 'archive(native) = "'$(notdir $(wildcard ./*.cmxa))'"' >> ./META


uninstall: 
	rm -f $(addprefix $(BINDIR)/,$(basename $(notdir $(TARGETS))))
	rm -f $(addsuffix .opt,$(addprefix $(BINDIR)/,$(basename $(notdir $(TARGETS)))))

uninstall_lib:
	@for i in $(notdir $(realpath $(filter-out ./,$(INCLUDEDIRS)))) $(notdir $(basename $(wildcard ./*.ml))); do \
		($(OCAMLFIND) remove -destdir $(LIBDIR) $$i) \
	done

# Documentation generation

.PHONY: doc html_doc latex_doc
doc:	$(HTML_GEN) $(LATEX_GEN) doc_html doc_latex

doc_html: $(MAIN_OBJS)
	mkdir -p $(HTML_DOCDIR)
	$(OCAMLDOC) $(OCAMLHTMLDOCFLAGS) $(filter %.ml %.mli,$(SOURCES)) $(filter %.ml %.mli,$(TARGETS))

doc_latex: $(MAIN_OBJS)
	mkdir -p $(LATEX_DOCDIR)
	$(OCAMLDOC) $(OCAMLLATEXDOCFLAGS) $(filter %.ml %.mli,$(SOURCES)) $(filter %.ml %.mli,$(TARGETS))

$(LATEX_GEN): $(DOCDIR)/$(LATEX_GEN)
	cd $(DOCDIR); make lib

$(HTML_GEN):
	cd $(DOCDIR); make lib

# Clean up

.PHONY: clean clean_objs clean_byte clean_native clean_depend clean_doc distclean
clean: distclean clean_doc uninstall

distclean: clean_byte clean_native clean_depend

clean_objs: 
	rm -f $(addsuffix *.cm[ioxa],$(INCLUDEDIRS))
	rm -f $(addsuffix *.cmxa,$(INCLUDEDIRS))
	rm -f $(addsuffix *.[ao],$(INCLUDEDIRS))
	rm -f $(addsuffix .ml,$(basename $(filter %.mll,$(SOURCES))))
	rm -f $(addsuffix .ml,$(basename $(filter %.mly,$(SOURCES))))
	rm -f $(addsuffix .mli,$(basename $(filter %.mly,$(SOURCES))))

clean_byte: clean_objs
	rm -f $(basename $(notdir $(TARGETS)))

clean_native: clean_objs
	rm -f $(addsuffix .opt,$(basename $(notdir $(TARGETS))))

clean_depend: 
	rm -f .depend

clean_doc:
	$(LATEX_DOC_RM_COMMAND)
	$(HTML_DOC_RM_COMMAND)

# Dependencies
.depend: 
	$(OCAMLFIND) $(OCAMLDEP) $(OCAMLFINDFLAGS) $(INCLUDES) $(addsuffix *.ml,$(INCLUDEDIRS)) $(addsuffix *.mli,$(INCLUDEDIRS))> .depend

# include .depend

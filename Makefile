# Makefile: makefile for SKK Dictionaries.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>

COUNT	  = skkdic-count
CURL      = curl
DATE	  = date
EMACS	  = emacs --batch --directory ./
EXPR	  = skkdic-expr
EXPR2	  = skkdic-expr2
GAWK	  = LC_ALL=C gawk
GREP	  = grep
SED	  = sed
GZIP	  = gzip -9
MD5	  = md5
MV	  = mv --force
RM	  = /bin/rm -f
RUBY	  = ruby -I $(TOOLS_DIR)/filters
SORT	  = skkdic-sort
TAR	  = tar
UNZIP	  = unzip -o
ZIPDIC_DIR  = ./zipcode

DIC2PDB = dic2pdb
DICCOMPACT = diccompact.rb
KANADIC2ROMADIC = kanadic2romadic
NKF = nkf
SKKDIC2KANADIC = skkdic2kanadic
TOOLS_DIR = ../tools

SRCS	  = SKK-JISYO.L SKK-JISYO.ML SKK-JISYO.M SKK-JISYO.S SKK-JISYO.JIS2 \
		SKK-JISYO.JIS3_4 SKK-JISYO.pubdic+ SKK-JISYO.wrong.annotated \
		SKK-JISYO.okinawa SKK-JISYO.geo SKK-JISYO.jinmei SKK-JISYO.law \
		SKK-JISYO.mazegaki SKK-JISYO.assoc SKK-JISYO.itaiji \
		SKK-JISYO.itaiji.JIS3_4 SKK-JISYO.china_taiwan \
		SKK-JISYO.propernoun SKK-JISYO.station SKK-JISYO.requested \
		SKK-JISYO.fullname SKK-JISYO.JIS2004 SKK-JISYO.lisp
# SKK-JISYO.noregist SKK-JISYO.hukugougo
BIN_SRCS  = #PBinlineDB.pdb
ALL_SRCS  = $(SRCS) $(BIN_SRCS) SKK-JISYO.wrong SKK-JISYO.L.unannotated
# SKK-JISYO.L+ SKK-JISYO.L.taciturn SKK-JISYO.total

PYTHON    = python
SKK2CDB   = skk2cdb.py -f
CDB_SOURCE = ./SKK-JISYO.L
CDB_TARGET = ./`basename $(CDB_SOURCE)`.cdb

clean:
	$(RM) *.gz* *~ `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'` \
	*.unannotated SKK-JISYO.wrong PBinlineDB.pdb *.tmp *.w PBinlineDB.dic *.taciturn SKK-JISYO.L+ SKK-JISYO.total SKK-JISYO.total+zipcode SKK-JISYO.L.header SKK-JISYO.china_taiwan \
	emoji-list.txt

archive: gzip

unannotated: SKK-JISYO.L.unannotated SKK-JISYO.wrong SKK-JISYO.china_taiwan.unannotated

SKK-JISYO.L.unannotated: SKK-JISYO.L
	$(GAWK) -f $(TOOLS_DIR)/unannotation.awk SKK-JISYO.L > SKK-JISYO.L.unannotated

SKK-JISYO.wrong: SKK-JISYO.wrong.annotated
	$(GAWK) -f $(TOOLS_DIR)/unannotation.awk SKK-JISYO.wrong.annotated > SKK-JISYO.wrong

SKK-JISYO.china_taiwan: csv/china_taiwan.csv
	$(RUBY) $(TOOLS_DIR)/convert2skk/ctdicconv.rb csv/china_taiwan.csv > SKK-JISYO.tmp
	$(EXPR) SKK-JISYO.tmp | $(SORT) - > SKK-JISYO.1.tmp
	cat SKK-JISYO.china_taiwan.header SKK-JISYO.1.tmp > SKK-JISYO.china_taiwan
	$(RM) SKK-JISYO.tmp SKK-JISYO.1.tmp

SKK-JISYO.china_taiwan.unannotated: SKK-JISYO.china_taiwan csv/china_taiwan.csv
	$(GAWK) -f $(TOOLS_DIR)/unannotation.awk SKK-JISYO.china_taiwan > SKK-JISYO.china_taiwan.unannotated

wrong_check: SKK-JISYO.wrong
	for file in $(SRCS) ; do \
	  if [ $$file != "SKK-JISYO.wrong.annotated" ] ; then \
	    $(EXPR) $$file - SKK-JISYO.wrong > $$file.tmp ;\
	    $(EXPR) $$file - $$file.tmp > $$file.w ;\
	    $(RM) $$file.tmp ;\
	    $(COUNT) $$file.w | $(GREP) -v '0 candidate' | \
	      sed -e 's/\.w:/:/' -e 's/\([1-9]\)/\1 wrong/' ;\
	    if [ $$? != 0 ]; then \
	      $(RM) $$file.w ; \
	    fi ;\
	  fi ;\
	done

PBinlineDB.dic: clean SKK-JISYO.L.unannotated
	$(SKKDIC2KANADIC) SKK-JISYO.L.unannotated | $(KANADIC2ROMADIC) - | $(NKF) -s > PBinlineDB.dic

PBinlineDB_compact.pdb: PBinlineDB.dic
	 $(DICCOMPACT) PBinlineDB.dic | $(DIC2PDB) - PBinlineDB.pdb

PBinlineDB_full.pdb: PBinlineDB.dic
	$(DIC2PDB) PBinlineDB.dic PBinlineDB.pdb

PBinlineDB.pdb: PBinlineDB_full.pdb
	$(RM) PBinlineDB.dic

gzip: clean $(ALL_SRCS)
	for file in $(ALL_SRCS); do \
	  $(GZIP) -fc $$file >$$file.gz ;\
	  $(MD5) $$file.gz >$$file.gz.md5; \
	done
	$(TAR) cvpf SKK-JISYO.edict.tar SKK-JISYO.edict edict_doc.html
	$(GZIP) -f SKK-JISYO.edict.tar
	$(MD5) SKK-JISYO.edict.tar.gz > SKK-JISYO.edict.tar.gz.md5
	$(TAR) cvzpf zipcode.tar.gz --exclude-from=./skk.ex ./zipcode
	$(MD5) zipcode.tar.gz >zipcode.tar.gz.md5

SKK-JISYO.L+: SKK-JISYO.L SKK-JISYO.L.header
	$(RUBY) $(TOOLS_DIR)/filters/conjugation.rb -Cpox SKK-JISYO.notes > SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/asayaKe.rb -p SKK-JISYO.L >> SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/complete-numerative.rb -pU SKK-JISYO.L >> SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/abbrev-convert.rb -K -s 2 SKK-JISYO.L >> SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/abbrev-convert.rb -w -s 2 SKK-JISYO.L >> SKK-JISYO.tmp
	$(EXPR2) SKK-JISYO.L + SKK-JISYO.tmp | cat SKK-JISYO.L.header - > SKK-JISYO.L+
	$(RM) SKK-JISYO.tmp SKK-JISYO.addition

SKK-JISYO.total: SKK-JISYO.L SKK-JISYO.geo SKK-JISYO.station SKK-JISYO.jinmei SKK-JISYO.propernoun SKK-JISYO.fullname SKK-JISYO.law SKK-JISYO.okinawa SKK-JISYO.hukugougo SKK-JISYO.assoc SKK-JISYO.notes SKK-JISYO.L.header
	$(RUBY) $(TOOLS_DIR)/filters/conjugation.rb -Cpox SKK-JISYO.notes > SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/asayaKe.rb -p SKK-JISYO.L >> SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/complete-numerative.rb -pU SKK-JISYO.L >> SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/abbrev-convert.rb -K -s 2 SKK-JISYO.L >> SKK-JISYO.tmp
	$(RUBY) $(TOOLS_DIR)/filters/abbrev-convert.rb -w -s 2 SKK-JISYO.L >> SKK-JISYO.tmp
	# order is very important here
	$(EXPR2) SKK-JISYO.geo + SKK-JISYO.station + SKK-JISYO.jinmei + SKK-JISYO.propernoun + SKK-JISYO.fullname + SKK-JISYO.tmp + SKK-JISYO.law + SKK-JISYO.okinawa + SKK-JISYO.hukugougo + SKK-JISYO.assoc - SKK-JISYO.L > SKK-JISYO.addition
	# why eliminating SKK-JISYO.L once? -- to not add too noisy
	# annotations from SKK-JISYO.jinmei and so on.
	$(EXPR2) SKK-JISYO.L + SKK-JISYO.addition | cat SKK-JISYO.L.header - > SKK-JISYO.total
	$(RM) SKK-JISYO.tmp SKK-JISYO.addition

SKK-JISYO.total+zipcode: SKK-JISYO.total $(ZIPDIC_DIR)/SKK-JISYO.zipcode $(ZIPDIC_DIR)/SKK-JISYO.office.zipcode SKK-JISYO.L.header
	$(EXPR2) SKK-JISYO.total + $(ZIPDIC_DIR)/SKK-JISYO.zipcode + $(ZIPDIC_DIR)/SKK-JISYO.office.zipcode | cat SKK-JISYO.L.header - > SKK-JISYO.total+zipcode

SKK-JISYO.L.taciturn: SKK-JISYO.L SKK-JISYO.L.header
	$(RUBY) $(TOOLS_DIR)/filters/annotation-filter.rb -d SKK-JISYO.L | $(EXPR2) | cat SKK-JISYO.L.header - > SKK-JISYO.L.taciturn

SKK-JISYO.L+.taciturn: SKK-JISYO.L+ SKK-JISYO.L.header
	$(RUBY) $(TOOLS_DIR)/filters/annotation-filter.rb -d SKK-JISYO.L+ | $(EXPR2) | cat SKK-JISYO.L.header - > SKK-JISYO.L+.taciturn

SKK-JISYO.total.taciturn: SKK-JISYO.total SKK-JISYO.L.header
	$(RUBY) $(TOOLS_DIR)/filters/annotation-filter.rb -d SKK-JISYO.total | $(EXPR2) | cat SKK-JISYO.L.header - > SKK-JISYO.total.taciturn

SKK-JISYO.total+zipcode.taciturn: SKK-JISYO.total+zipcode SKK-JISYO.L.header
	$(RUBY) $(TOOLS_DIR)/filters/annotation-filter.rb -d SKK-JISYO.total+zipcode | $(EXPR2) | cat SKK-JISYO.L.header - > SKK-JISYO.total+zipcode.taciturn

SKK-JISYO.L+.unannotated: SKK-JISYO.L+
	$(GAWK) -f $(TOOLS_DIR)/unannotation.awk SKK-JISYO.L+ > SKK-JISYO.L+.unannotated

SKK-JISYO.total.unannotated: SKK-JISYO.total
	$(GAWK) -f $(TOOLS_DIR)/unannotation.awk SKK-JISYO.total > SKK-JISYO.total.unannotated

SKK-JISYO.total+zipcode.unannotated: SKK-JISYO.total+zipcode
	$(GAWK) -f $(TOOLS_DIR)/unannotation.awk SKK-JISYO.total+zipcode > SKK-JISYO.total+zipcode.unannotated

SKK-JISYO.L.header: SKK-JISYO.L
	echo ';; (This dictionary was automatically generated from SKK dictionaries)' > SKK-JISYO.L.header
	$(SED) -n '/^;; okuri-ari entries./q;p' SKK-JISYO.L >> SKK-JISYO.L.header

unannotated-all: unannotated SKK-JISYO.L+.unannotated SKK-JISYO.total.unannotated SKK-JISYO.total+zipcode.unannotated

taciturn-all: SKK-JISYO.L.taciturn SKK-JISYO.L+.taciturn SKK-JISYO.total.taciturn SKK-JISYO.total+zipcode.taciturn

annotated-all: SKK-JISYO.L+ SKK-JISYO.total SKK-JISYO.total+zipcode

all: annotated-all unannotated-all taciturn-all

cdb:
	$(PYTHON) $(TOOLS_DIR)/$(SKK2CDB) $(CDB_TARGET) $(CDB_SOURCE)


# Unicode emoji

VER = 36.1

SKK-JISYO.emoji: SKK-JISYO.emoji.en SKK-JISYO.emoji.ja SKK-JISYO.emoji.kana unicode-license.txt
	$(EXPR2) SKK-JISYO.emoji.en + SKK-JISYO.emoji.ja + SKK-JISYO.emoji.kana \
	  > SKK-JISYO.emoji.tmp
	echo '-*- mode: fundamental; coding: utf-8 -*-' | cat - unicode-license.txt | $(SED) "s/^/;; /g" | cat - SKK-JISYO.emoji.tmp > SKK-JISYO.emoji
	$(RM) SKK-JISYO.emoji.en SKK-JISYO.emoji.ja en.xml ja.xml
	$(RM) SKK-JISYO.emoji.tmp SKK-JISYO.emoji.kana

SKK-JISYO.emoji.en: cldr-common.zip
	test -f en.xml || $(UNZIP) -p cldr-common.zip "*common/annotations/en.xml" > en.xml
	$(EMACS) --load emoji.el --funcall en > SKK-JISYO.emoji.en

SKK-JISYO.emoji.ja: cldr-common.zip
	test -f ja.xml || $(UNZIP) -p cldr-common.zip "*common/annotations/ja.xml" > ja.xml
	$(EMACS) --load emoji.el --funcall ja > SKK-JISYO.emoji.ja

SKK-JISYO.emoji.kana: SKK-JISYO.emoji.kanji
	$(EMACS) --load emoji.el --funcall kanji-to-kana > SKK-JISYO.emoji.kana
	$(RM) SKK-JISYO.emoji.kanji

SKK-JISYO.emoji.kanji: cldr-common.zip
	test -f ja.xml || $(UNZIP) -p cldr-common.zip "*common/annotations/ja.xml" > ja.xml
	$(EMACS) --load emoji.el --funcall kanjionly | $(EXPR2) > SKK-JISYO.emoji.kanji

unicode-license.txt: cldr-common.zip
	test -f unicode-license.txt || $(UNZIP) -p cldr-common.zip "*unicode-license.txt" > unicode-license.txt

cldr-common.zip:
	test -f cldr-common.zip || $(CURL) -o cldr-common.zip https://unicode.org/Public/cldr/$(VER)/cldr-common-$(VER).zip

# http://www.edrdg.org/jmdict/edict.html
#   ELECTRONIC DICTIONARY RESEARCH AND DEVELOPMENT GROUP GENERAL DICTIONARY LICENCE STATEMENT
#   http://www.edrdg.org/edrdg/licence.html

SKK-JISYO.edict2: edict2u
	$(MV) SKK-JISYO.edict2 SKK-JISYO.edict2.ORIG
	$(EMACS) --load ../tools/convert2skk/edict2toskk.el --funcall main | $(EXPR2) > SKK-JISYO.edict2.tmp
	$(EMACS) --load ../tools/convert2skk/edict2toskk.el --funcall after
	$(MV) SKK-JISYO.edict2.tmp SKK-JISYO.edict2
	$(GZIP) -fc SKK-JISYO.edict2 > SKK-JISYO.edict2.gz
	$(MD5) SKK-JISYO.edict2.gz > SKK-JISYO.edict2.gz.md5

edict2u:
	$(CURL) -o edict2u.gz http://ftp.monash.edu/pub/nihongo/edict2u.gz
	$(GZIP) --force --decompress edict2u.gz


# Unicode Ideographic Variation Database (IVD)

SKK-JISYO.ivd: IVD_Sequences.txt IVD_Collections.txt
	$(EMACS) --load ivd.el --funcall make-ivd-jisyo | $(EXPR2) > SKK-JISYO.ivd.tmp
	echo '-*- mode: fundamental; coding: utf-8 -*-' | cat - unicode-license.txt | $(SED) "s/^/;; /g" | cat - SKK-JISYO.ivd.tmp > SKK-JISYO.ivd
	$(RM) SKK-JISYO.ivd.tmp

IVD_Sequences.txt:
	test -f IVD_Sequences.txt || $(CURL) -o IVD_Sequences.txt https://unicode.org/ivd/data/2017-12-12/IVD_Sequences.txt

IVD_Collections.txt:
	test -f IVD_Collections.txt || $(CURL) -o IVD_Collections.txt https://unicode.org/ivd/data/2017-12-12/IVD_Collections.txt

# end of Makefile.

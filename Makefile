#

TOP     = $(PWD)

TEST_DIR = tests
TEST_BIN = $(TEST_DIR)/bin
TEST_TAR = tests.tar.xz

INSTALL	= install
STRIP	= -s

all:
	# build fsx-linux
	make -C fsx-linux all
	$(INSTALL) -c -m 755 -d $(TEST_BIN)
	$(INSTALL) $(STRIP) -m 755 fsx-linux/fsx-linux $(TEST_BIN)

	# build fsstress
	make -C fsstress all
	$(INSTALL) -c -m 755 -d $(TEST_BIN)
	$(INSTALL) $(STRIP) -m 755 fsstress/fsstress $(TEST_BIN)

	# make tarball
	tar cJf $(TEST_TAR) $(TEST_DIR)

clean:
	rm -rf $(TEST_TAR) $(TEST_BIN)
	make -C fsx-linux clean
	make -C fsstress clean

## Geotab RPGLE SDK

This an SDK for the Geotab APIs.

Currently supported Geotab APIs:

* Auth
* Get
* GetFeed
* GetRoadMaxSpeeds
* GetAddresses
* Add
* Set

Please see the examples.

### Installation

The GEOTAB service program uses [noxDb](https://github.com/sitemule/noxDB) as the JSON manipulation library. We ship it in the repo so the users do not have to also build that.

#### Install service programs

1. `CRTLIB GEOTAB`
2. Upload the contents of the `pkgs` folder to the IFS
3. Unpack `JSONXML`: `CPYFRMSTMF FROMSTMF('./pkgs/jsonxml.pkg') TOMBR('/QSYS.LIB/GEOTAB.LIB/PACKAGE.FILE') MBROPT(*REPLACE) CVTDTA(*NONE)`
4. Restore `JSONXML`: `RST DEV('/QSYS.LIB/GEOTAB.LIB/PACKAGE.FILE') OBJ(('/QSYS.LIB/GEOTAB.LIB/JSONXML.SRVPGM'))`
5. Delete the `PACKAGE` save file: `DLTOBJ OBJ(GEOTAB/PACKAGE) OBJTYPE(*FILE)`
6. Unpack `JSONXML`: `CPYFRMSTMF FROMSTMF('./pkgs/geotab.pkg') TOMBR('/QSYS.LIB/GEOTAB.LIB/PACKAGE.FILE') MBROPT(*REPLACE) CVTDTA(*NONE)`
7. Restore `JSONXML`: `RST DEV('/QSYS.LIB/GEOTAB.LIB/PACKAGE.FILE') OBJ(('/QSYS.LIB/GEOTAB.LIB/GEOTAB.SRVPGM'))`
8. Delete the `PACKAGE` save file: `DLTOBJ OBJ(GEOTAB/PACKAGE) OBJTYPE(*FILE)`
9. Copy `headers/geotab.rpgle_h` into `GEOTAB/QRPGLEREF` since they are the header files
10. Create a binding directory with both `GEOTAB` and `JSONXML` on.

#### Build from source

1. Create the library in 5250: `CRTLIB GEOTAB`
2. Clone & `cd geotab_rpgle_sdk`
3. Restore JSONXML: `gmake jsonxml.restore`
4. Build the service program: `gmake` (this also creates a binding directory and `QRPGLEREF` with the header file)
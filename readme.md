## Geotab RPGLE SDK

This an SDK for the Geotab APIs.

Currently supported Geotab APIs:

* Auth
* Get
* GetFeed

Please see the examples.

### Installation

The GEOTAB service program uses [noxDb](https://github.com/sitemule/noxDB) as the JSON manipulation library. We ship it in the repo so the users do not have to also build that.

#### Build from source

1. Create the library in 5250: `CRTLIB GEOTAB`
2. Clone & `cd geotab_rpgle_sdk`
3. Restore JSONXML: `gmake jsonxml.restore`
4. Build the service program: `gmake` (this also creates a binding directory and `QRPGLEREF` with the header file)
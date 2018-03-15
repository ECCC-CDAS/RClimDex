RClimDex
========

* RClimDex is a library that provides a friendly GUI in R for computing the 27 core indices of extreme climate defined by ETCCDI. It also conducts simple quality control on the input daily data.

Links
-----

* `27 core indices of extreme climate`_
* `R statistical programming language`_

.. _27 core indices of extreme climate: http://etccdi.pacificclimate.org/list_27_indices.shtml
.. _R statistical programming language: http://www.r-project.org/

Using RClimDex
==============

Installing R
------------

* RClimDex requires the base package of R (Version 2.15.2 or later). The installation of R involves a very simple procedure. First, connect to the `R project website`_, then follow the links to download the most recent version of R for your computer operating system from any mirror site of CRAN.

.. _R project website: http://www.r-project.org/

Installing RClimDex
-------------------

* The latest version of RClimDex can be acquire in the `release tab`_. Please install RClimDex as a local package in R. RClimDex now depends on the climdex.pcic. With an internet connection, launch R in the same directory as the RClimDex package. Then run the following commands ::

    > install.packages("climdex.pcic")
    > install.packages(install.packages("RClimDex_1.9-2.tar.gz", repos=NULL, type="source")

.. _release tab: https://github.com/rodneychan-ec/RClimDex/releases

Running RClimDex
----------------

* Run the following command to start ::

    > rclimdex.start()

Help
----

* Please read the `manual`_

.. _manual: https://github.com/rodneychan-ec/RClimDex/tree/master/inst/doc/manual.pdf

Issues
------

* Please check the `issue page`_ and check if the issue is already reported and its current status.
* If the issue is not reported yet, please kindly submit a `new issue`_, tag the issue as bug and leave it unassigned. Please describe your issue in as much detail as possible and to include your output.

.. _issue page: https://github.com/rodneychan-ec/RClimDex/issues
.. _new issue: https://github.com/rodneychan-ec/RClimDex/issues/new

Contact Us
----------

* I am Rodney Chan and the current maintainer of RClimDex. You can contact me at rodney.chan@canada.ca

# TDB-XMLT
Development of format for Calphad databases

This is the repository for the development of an XML type database format of Calphad databases

The Calphad databases contain thermodynamic model parameters for assessed systems such as Fe-C, Ni-Al etc normally published in scientific papers.  These assessments are fitted to experimental and theoretical data and use models to describe the dependence on T, P and constitution of the phases.  Currently various dialects of the TDB format is used for most such databases and this include bibliographic references to the assessments.  A Calphad database with 5-10 elements is extremly valuable for scientific work as well as for industry because the model parameters not only describe phase diagrams and other thermodynamic properties for stable systems, the databases are mainly used for simulating phase transformations providing phase amounts and compositions, chemical potentials and much more because the database include descriptions of metastable states of the phases.

Using the TDB format the manager maintaining such a database, commercial or free, have no tools for testing the internal consistency of the database, there can be duplicate paremeters, spelling errors etc.  Thus the use of a formal markup language such as XML would be useful.  The database manager normally also make modifications and additions to the published assessments, in particular to imporve extrapolations to multicomponent system, and such modifications should also be documented for future updates (by maybe another database manager).

The project aims to develop a format, called XMLT, for handling thermodynamic as well as kinetic and other model parameters specific to phases and elements in the database.  It should be able to handle all types of model for T, P and constitution of the phases.

The project will also provide an APPEND software which can merge an XMLT file to a master XMLT database including necessary consistency checks and handling of duplicates, for example a phase may be modeled with two or more different models.

There are several software using dialects of the original TDB format and each software should develop an UPLOAD/DOWNLOD software which should be able to converts its own TDB dialect to the XMLT format or convert a subset (or whole) of an XMLT format to its own TDB dialect.

This means there are no restrictions on the current TDB dialects and the XMLT is mainly a tool for database managers to make it possible to maintain the integrity of the database and provide documentation of all their efforts to make the database useful.

The first version of the XMLT format is open to extentions but a governing body should check and approve/disapprove such extentions.

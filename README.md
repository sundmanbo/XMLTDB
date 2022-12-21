# TDB-XMLT
Development of format for Calphad databases

This is the repository for the development of an XML type database format of Calphad databases

The Calphad databases contain thermodynamic model parameters for assessed systems such as Fe-C, Ni-Al etc normally published in scientific papers.  These assessments are fitted to experimental and theoretical data and use models to describe the dependence on T, P and constitution of the phases.  Currently various dialects of the TDB format is used for most such databases and this include bibliographic references to the assessments.  A large Calphad database with 10-15 elements is extremly valuable for scientific work as well as for industry because the model parameters not only describe phase diagrams and other thermodynamic properties such as chemical potentials and heat capacites for stable systems because they are used for simulating phase transformations as they include descriptions of metastable states.

Using the TDB format those responsible to maintain such databases, commercial or free, lack methods for testing the internal consistency of the database, there can be duplicate paremeters, spelling errors etc and thus the use of a formal markup language such as XML would be useful.  The database manager normally also make modifications and addtitions to the published assessments, in particular to imporve extrapolations to multicomponent system, and such modifications should also be documented for future updates by maybe another database manager.

The project aims to develop a format, called XMLT, for handling thermodynamic as well as kinetic and other model parameters specific to phases and elements in the database.  It should be able to handle all types of model for T, P and constitution of the phases.

The project will provide an APPEND software which can merge an XMLT file to a master XMLT database including necessary consistency checks and handling of duplicates, for example ome phases may be modeled with two or more different models.
There are several software using dialects of the original TDB format and each software should develop an UPLOAD/DOWNLOD software which should be able to converts its own TDB dialect to the XMLT format or convert a subset (or whole) of an XMLT format to its own TDB dialect.

The XMLT format is open to extentions but a governing body should check and approve/disapprove such extentions.

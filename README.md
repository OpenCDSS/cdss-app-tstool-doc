# cdss-app-tstool-doc #

This repository contains the legacy Word/PDF TSTool software documentation,
including user manual, training materials, and a variety of developer resources.

**This documentation is being phased out in favor of new Markdown/MkDocs documentation.
In general, do not edit this documentation and only use as a reference when migrating into the newer format.**
See the
[TSTool Developer Documentation](https://github.com/OpenWaterFoundation/cdss-app-tstool-doc-dev) and
[TSTool User Documentation](https://github.com/OpenWaterFoundation/cdss-app-tstool-doc-user) repositories.

See the following online resources:

* [CDSS](http://cdss.state.co.us)
* [OpenCDSS](http://learn.openwaterfoundation.org/cdss-emod-dev/)
* [TSTool Developer Documentation](http://learn.openwaterfoundation.org/cdss-app-tstool-doc-dev/)
* [TSTool User Documentation](http://learn.openwaterfoundation.org/cdss-app-tstool-doc-user/)

The developer documentation and guidelines will be updated as the development environment is used in development.  See the following sections in this page:

* [Repository Folder Structure](#repository-folder-structure)
* [Repository Dependencies](#repository-dependencies)
* [Development Environment Folder Structure](#development-environment-folder-structure)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)

-----

## Repository Folder Structure ##

The following are the main folders and files in this repository, listed alphabetically.
See also the [Development Environment Folder Structure](#development-environment-folder-structure)
for overall folder structure recommendations.

```
cdss-app-tstool-doc/          TSTool Word/PDF documentation files.
  .git/                       Git repository folder (DO NOT MODIFY THIS except with Git tools).
  .gitattributes              Git configuration file for repository.
  .gitignore                  Git configuration file for repository.
  .project                    Eclipse configuration file (general file project).
  doc/                        Documentation files.
    DeveloperResources/       Developer resources.
    HowTo/                    Helpful notes.
    QuickStart/               Examples of how to use TSTool.
    Training/                 Training lessons.
    UserManual/               User manual files.
  LICENSE.txt                 TSTool documentation license file.
  README.md                   This file.
  test/                       Functional tests used in documentation examples.
```

## Repository Dependencies ##

Repository dependencies fall into two categories as indicated below.

### TSTool Documentation Repository Dependencies ###

None, although more information will be added here later about PDF merge utilities.

### Repositories that Depend on This Repository ###

|**Repository**|**Description**|
|-------------------------------------------------------------------------------------|----------------------------------------|
|[`cdss-app-tstool-main`](https://github.com/OpenWaterFoundation/cdss-app-tstool-main)|Main TSTool software repository/project.|

## Development Environment Folder Structure ##

See the [TSTool main software repository](https://github.com/OpenWaterFoundation/cdss-app-tstool-main)
for information about the TSTool development environment folder structure.

## Contributing ##

Contributions to this project can be submitted using the following options:

1. TSTool software developers with commit privileges can write to this repository
as per normal OpenCDSS development protocols.
2. Post an issue on GitHub with suggested change.  Provide information using the issue template.
3. Email a development contact.
4. Fork the repository, make changes, and do a pull request.
Contents of the current master branch should be merged with the fork to minimize
code review before committing the pull request.

See also the [OpenCDSS / TSTool protocols](http://learn.openwaterfoundation.org/cdss-website-opencdss/tstool/tstool/).

## License ##

A license for the software is being determined as part of the OpenCDSS project.
GPL 3.0 has been recommended.

## Contact ##

See the [OpenCDSS TSTool information for product contacts](http://learn.openwaterfoundation.org/cdss-website-opencdss/tstool/tstool/#product-leadership).

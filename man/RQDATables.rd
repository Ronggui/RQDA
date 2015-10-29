%dbListFields(RQDA:::.rqda$qdacon,"attributes")
\name{RQDATables}
\alias{RQDATables}
\title{Data Tables in rqda file}
\description{
  The internal data table structures in rqda file, which is a SQLite data base.
}
\details{
  Table "annotation" contatins file annotations.
   \tabular{ll}{
   fid:\tab file id. \cr 
   position:\tab position of annotation.\cr
   annotation:\tab content of annotation.\cr
   owner:\tab owner of annotation.\cr
   date:\tab created date.\cr
   dateM:\tab not used currently.\cr
   status:\tab 1 for standard status and 0 for temporarily deleted annotation.\cr
   }

  Table "attributes" contatins information about the name list of
  attributes. They are held in the widget of ".AttrNamesWidget".
  \tabular{ll}{
    name:\tab name of attributes. \cr 
    status:\tab 1 for standard status and 0 for a temporarily deleted attribute.\cr 
    date:\tab created date of as attribute.\cr 
    dateM:\tab not used currently.\cr 
    owner:\tab owner of an attribute. \cr 
    memo:\tab memo of an attribute. Useful for definition of attributes.\cr
    class:\tab class of an attribute. It might be "character" or "numeric".\cr
  }
  
  Table "caseAttr" contatins information about attributes of cases.
  \tabular{ll}{
    variable:\tab name of case attributes, coresponding to name in
    attributes table.\cr
    value:\tab variable value.\cr 
    caseID:\tab corresponding case id of a variable value.\cr 
    date:\tab created date of a case attribute record.\cr 
    dateM:\tab not used currently.\cr 
    owner:\tab creator of the case attribute record.\cr 
  }
  
  Table "caselinkage" contatins information about the relationship
  between case and files of case.
  \tabular{ll}{
    caseid:\tab case id. \cr 
    fid:\tab file id. \cr 
    selfirst:\tab beginning position of a text segment associated with a case. \cr 
    selend:\tab  ending position of a text segment associated with a case.. \cr 
    status:\tab 1 for standard status and 0 for temporarily deleted record. \cr 
    owner:\tab creator of the case linkage. \cr 
    date:\tab date of a created case linkage. \cr 
    memo:\tab not used currently. \cr 
  }
  
  Table "cases" contatins information about case list.
  \tabular{ll}{
    name:\tab name of a case. \cr 
    memo:\tab case memo. \cr 
    owner:\tab creator of a case. \cr 
    date:\tab date of creation of a case. \cr 
    dateM:\tab not used currently. \cr 
    id:\tab case id. \cr 
    status:\tab 1 for standard status and 0 for temporarily deleted record. \cr }
  
  Table "codecat" contatins information about upper-level of code list.
  \tabular{ll}{
    name:\tab name of code category. \cr 
    cid:\tab not used currently. \cr 
    catid:\tab id of code category. \cr 
    owner:\tab creator of code category. \cr 
    date:\tab date of creation of code category. \cr 
    dateM:\tab not used currently. \cr 
    memo:\tab code category memo. \cr 
    status:\tab 1 for standard status and 0 for temporarily deleted record. \cr 
  }
  
  Table "coding" contains information on codings.
  \tabular{ll}{
    cid :\tab code id.\cr 
    fid :\tab file id.\cr 
    seltext :\tab a coding, that is the coded text segment.\cr 
    selfirst :\tab beginning position of the coded text segment.\cr 
    selend :\tab ending position of the coded text segment.\cr 
    status :\tab 1 for standard status. 0 for deleted codeings (for
  example when a code is deleted, the status of all associated codings
  is set to 0) and -1 for unmarked codings.\cr 
    owner :\tab name of coder or creator of a coding. \cr 
    date :\tab date of creation of a coding.\cr 
    memo :\tab coding memo.\cr 
  }

  Table "fileAttr" contatins information about attributes of files.
  \tabular{ll}{
    variable:\tab charater, name of file attribute, coresponding to name in
    attributes table \cr 
    value:\tab value of the file attribute. \cr 
    fileID:\tab corresponding file id of the attribute. \cr 
    date:\tab created date of the file attribute.\cr 
    dateM:\tab not used currently. \cr 
    owner:\tab creator of the file attribute. \cr 
  }
  
  Table "filecat" contains information on the file categorization.
  \tabular{ll}{
    name:\tab name of the file category.\cr
    fid:\tab Not used.\cr
    catid:\tab if of file category.\cr
    owner:\tab creator of file-category.\cr
    date:\tab date of creation of a file category.\cr
    dateM:\tab not used currently.\cr
    memo:\tab file category memo.\cr
    status:\tab 1 for standard status and 0 for temporarily deleted record. \cr
  }

  Table "freecode" contains information on the codes list.
  \tabular{ll}{
    name :\tab code name.\cr 
    memo :\tab code memo.\cr 
    owner :\tab creator of a code.\cr 
    date :\tab date of creation of a code.\cr 
    dateM :\tab not used currently.\cr 
    id :\tab code id.\cr 
    status :\tab 1 for standard status and 0 for temporarily deleted record.\cr
    color:\tab color for code marker (added in version 0.19)\cr
  }

  Table "image" contatins information about images. It is not used currently.

  Table "imageCoding" contatins images coding. It is not used currently.
  
  Table "journal" contatins information about field work
  journal. Journal titles are held in widget of ".JournalNamesWidget".
  \tabular{ll}{
    name:\tab name of a journal. \cr 
    journal:\tab content of a journal. \cr 
    date:\tab created date of a journal.\cr 
    dateM:\tab not used currently. \cr 
    owner:\tab owner of a journal.\cr 
    status:\tab 1 for standard status and 0 for temporarily deleted journal. \cr 
  }
  
  Table "project" contatins information about the project and *.rqda file.
  \tabular{ll}{
    encoding:\tab not used currently.\cr 
    databaseversion:\tab version of RQDAtables. \cr 
    date:\tab created date of the project. \cr 
    dateM:\tab not used currently.\cr 
    memo:\tab project memo.\cr 
    BOM:\tab not used curently.\cr 
    imageDir:\tab directory of image. Not used currently.\cr
    about:\tab meta information about the rqda file.\cr
  }
  
  Table "source" contains the content of files. Files are held in widget
  of ".fnames_rqda".
  \tabular{ll}{
    name:\tab name of the file.\cr
    id:\tab id of the file.\cr
    file:\tab content of a file.\cr
    memo:\tab memo of the file.\cr
    owner:\tab creator the the file.\cr
    date:\tab the date of the file-import.\cr
    dataM:\tab date of last editing of the file content.\cr
    status:\tab 1 for standard status and 0 for temporarily deleted file.\cr
  }
  
  The "treecode" table contains information on the codes categorization
  (relationship between codes and the codecat). They are held in widget
  of ".CodeCatWidget". Codes of specific category are held in widget of ".CodeofCat".
  \tabular{ll}{
    cid:\tab code id. \cr 
    catid:\tab code category id. \cr 
    date:\tab date of creation of a code categorization. \cr 
    dateM:\tab not used currently. \cr 
    memo:\tab not used currently. \cr 
    status:\tab 1 for standard status and 0 for temporarily deleted
    file. \cr 
    owner:\tab creator the the treecode.\cr
  }

  Table "treefile" contatins information about file categorization
  (relation between source files and filecat).
  \tabular{ll}{
    fid:\tab file id. \cr 
    catid:\tab file category id. \cr 
    date:\tab date of creation of the file categorization. \cr 
    dateM:\tab not used currently. \cr 
    memo:\tab not used currently. \cr 
    status:\tab 1 for standard status and 0 for temporarily deleted
    record. \cr 
    owner:\tab creator the the tree file.\cr
  }
  
}
\author{ HUANG Ronggui }

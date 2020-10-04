# Mendelian inheritance

[![Build Status](https://travis-ci.org/iu-haskell-spring-2020/project-template.svg?branch=master)](https://travis-ci.org/iu-haskell-spring-2020/project-template)

Haskell library presenting an ergonomic way to work with genetics problems using Mendelian inheritance laws.

Mendelian inheritance allows one to predict traits for future generations based
on the following laws:

* **Law of Segregation of genes** (the “First Law”):
every individual organism contains two alleles for each trait, and that
these alleles segregate (separate) during meiosis (cell division) such
that each gamete contains only one of the alleles. Each offspring thus
receives a pair of alleles for a trait by inheriting one allele from each
parent.

* **Law of Independent Assortment** (the “Second Law”):
alleles for separate traits are passed independently of one another.

* **Law of Dominance** (the “Third Law”):
recessive alleles will always be masked by dominant alleles. When both types of alleles are present, only a trait encoded by the dominant one will show. So if one of the parents has two dominant alleles, then first generation of offsprings is guaranteed to have a dominant trait.

In the image above the alleles are represented using capital and small letters. For instance capital S (short tail) represents an allele encoding the dominant short tail trait and small s represents a recessive allele for a long tail. And capital B (brown) represents a dominant allele encoding brown fur color while small b represents a recessive allele for white fur color.

**Genotype** is a set of alleles for all traits (e.g., AAbbCc).
**Phenotype** is a set of actual traits that an organism has (e.g., AbC).

## Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.6.5).

## Run

This project has one executable that you can run with

```
stack exec mendelian-exe
```

During development it is recommended a combination of `build` and `exec`:

```
stack build && stack exec mendelian-exe
```

Alternatively, you can run

```
stack build file-watch
```

For continuous builds in the background.

## Usage

The following commands are implemented for user communication with the library:

```
/gen <label> <trait>
```
The command adds new gen to the state for the further procession.

```
/allele <gen label> <trait expression>
```
The command adds new gen to the state for the further procession. Label should match with one of the gens presented via `/gen` command. Lower or upper case of the label signs the dominance of the allele.

```
/geno1 <genotype>
/geno2 <genotype>
```
The similar commands which add the genotype of a two initial parents. Genotype should consist of alleles' labels presented via `/allele` command.

```
/show 
```
The command shows the current state of provided gens, alleles and parents' genotypes.

```
/offs
```
Compute the first generation of children for the provided parents.

```
/exit
```
Exit the program.

Example is shown below:

'''
/gen a color
Gene added!
/gen b smoothness
Gene added!
/allele A green
Allele added!
/allele a yellow
Allele added!
/allele B smooth
Allele added!
/allele b wrinkle
Allele added!
/geno1 AaBb
Parent 1 genotype set!
/geno2 Aabb
Parent 2 genotype set!
/offs

--Population description--

Genotype: aabb
Genotype ratio: 0.125
Phenotype: ab
a: color yellow
b: smoothness wrinkle

Genotype: aaBb
Genotype ratio: 0.125
Phenotype: aB
a: color yellow
B: smoothness smooth

Genotype: Aabb
Genotype ratio: 0.25
Phenotype: Ab
A: color green
b: smoothness wrinkle

Genotype: AaBb
Genotype ratio: 0.25
Phenotype: AB
A: color green
B: smoothness smooth

Genotype: AAbb
Genotype ratio: 0.125
Phenotype: Ab
A: color green
b: smoothness wrinkle

Genotype: AABb
Genotype ratio: 0.125
Phenotype: AB
A: color green
B: smoothness smooth

/exit
'''

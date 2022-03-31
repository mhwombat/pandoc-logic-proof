# pandoc-logic-proof

A pandoc filter for writing and formatting logic proofs.

## Sample output

Sample PDF output:

![](pdf.png)

Sample HTML output for the same proof:

![](html.png)


## Usage

The [tutorial](tutorial.pdf) explains how to write proofs using the Markdown
syntax extensions provided by this filter.
Then, you can format your document either using pandoc directly,
or through Hakyll.

### With Pandoc

Use this filter by adding `--filter=pandoc-logic-proof` to your pandoc command.
For example:

    pandoc --filter=pandoc-logic-proof myfile.md --output=myfile.pdf

The tutorial was produced using the command:

    pandoc --filter=pandoc-logic-proof tutorial.md --output=tutorial.pdf

### With Hakyll

Use this filter as a transform in Hakyll.
For example, you could modify `site.hs`, adding

```
import Text.Pandoc.Filters.LogicProof (transform)
```

and changing

```
pandocCompiler
```

to

```
pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions transform
```


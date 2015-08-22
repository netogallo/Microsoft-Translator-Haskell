# Microsoft Translator

This is a simple interface to use [Microsoft
Translator](https://www.microsoft.com/en-us/translator/default.aspx)
in Haskell. To use this library you must set up an account at [Azure's
Data Marketplace](https://datamarket.azure.com/dataset/bing/microsofttranslator).

The simplest way to use Microsoft Translator is through the toplevel
translate function:

```
translate :: ClientId -> ClientSecret -> Text -> BingLanguage -> BingLanguage -> IO (Either BingError Text
```

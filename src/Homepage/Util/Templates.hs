module Homepage.Util.Templates
    ( getTemplates
    , getTemplate
    , templateToResponse
    , serveTemplate
    , serveTemplate' )
where

import Control.Monad.Reader

import Data.IORef
import Data.Maybe

import Homepage.Types

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Happstack.Server


badTemplate :: String -> Template
badTemplate nm = newSTMP $ "bad template: " ++ nm


getTemplateFromGroup :: String -> TemplateGroup -> Template
getTemplateFromGroup tmpl group =
    fromMaybe (badTemplate tmpl)
              (getStringTemplate tmpl group)


getTemplates :: HomepageMonad TemplateDirs
getTemplates = (ask >>= return . homepageTemplateMVar)
               >>= (\x -> liftIO $ readIORef x)


getTemplate :: String           -- ^ directory group
            -> String           -- ^ template name
            -> HomepageMonad Template
getTemplate grp nm =
    getTemplates
      >>= return . getTemplateFromGroup nm
                 . getTemplateGroup grp


templateToResponse :: Template -> HomepageHandler
templateToResponse = return . toResponse . BStoHTML . render


serveTemplate :: String         -- ^ directory group
              -> String         -- ^ template name
              -> HomepageHandler
serveTemplate grp nm = (lift $ getTemplate grp nm) >>= templateToResponse


serveTemplate' :: String                 -- ^ directory group
               -> String                 -- ^ template name
               -> (Template -> Template) -- ^ function to mutate the template
               -> HomepageHandler
serveTemplate' grp nm f =
    (lift $ getTemplate grp nm) >>= (templateToResponse . f)

module Homepage.Util.Templates
    ( getTemplates
    , getTemplate
    , templateToResponse
    , serveTemplate
    , serveTemplate' )
where

import Control.Concurrent.MVar
import Control.Monad.State.Strict

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
getTemplates = (get >>= return . homepageTemplateMVar)
               >>= (\x -> liftIO $ readMVar x)


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

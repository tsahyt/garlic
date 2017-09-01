{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Markdown.Persist where

import Database.Persist
import Database.Persist.Sql
import Text.Markdown

deriving instance PersistField Markdown
deriving instance PersistFieldSql Markdown

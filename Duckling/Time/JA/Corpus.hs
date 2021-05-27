-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.JA.Corpus
  ( corpus
--   , defaultCorpus
--   , negativeCorpus
--   , latentCorpus
--   , diffCorpus
  ) where

import Data.String
import Prelude

import Duckling.Core
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month, refTime)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
-- today
  [ examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "今日"
        
             ]
  -- yesterday
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "昨日"
             ]
  -- tomorrow
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "明日"
             ]
  ]
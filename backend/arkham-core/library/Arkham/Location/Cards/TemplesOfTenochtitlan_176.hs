module Arkham.Location.Cards.TemplesOfTenochtitlan_176
  ( templesOfTenochtitlan_176
  , TemplesOfTenochtitlan_176(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillTest
import Arkham.Timing qualified as Timing

newtype TemplesOfTenochtitlan_176 = TemplesOfTenochtitlan_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_176 :: LocationCard TemplesOfTenochtitlan_176
templesOfTenochtitlan_176 = locationWith
  TemplesOfTenochtitlan_176
  Cards.templesOfTenochtitlan_176
  3
  (PerPlayer 1)
  (labelL .~ "square")

instance HasModifiersFor TemplesOfTenochtitlan_176 where
  getModifiersFor target (TemplesOfTenochtitlan_176 a) | isTarget a target = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure []
      Just skillTest -> do
        healthMatches <- fieldP
          InvestigatorRemainingHealth
          (<= 3)
          (skillTestInvestigator skillTest)
        let
          isInvestigate =
            skillTestAction skillTest == Just Action.Investigate && isTarget
              a
              (skillTestTarget skillTest)
        pure $ toModifiers
          a
          [ ShroudModifier (-2) | healthMatches && isInvestigate ]
  getModifiersFor _ _ = pure []

instance HasAbilities TemplesOfTenochtitlan_176 where
  getAbilities (TemplesOfTenochtitlan_176 attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId attrs
    ]

instance RunMessage TemplesOfTenochtitlan_176 where
  runMessage msg l@(TemplesOfTenochtitlan_176 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      iids <- selectList $ investigatorAt (toId attrs)
      pushAll
        [ InvestigatorDirectDamage iid (toSource attrs) 1 0 | iid <- iids ]
      pure l
    _ -> TemplesOfTenochtitlan_176 <$> runMessage msg attrs

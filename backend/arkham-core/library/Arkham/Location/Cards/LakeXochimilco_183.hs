module Arkham.Location.Cards.LakeXochimilco_183
  ( lakeXochimilco_183
  , LakeXochimilco_183(..)
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

newtype LakeXochimilco_183 = LakeXochimilco_183 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lakeXochimilco_183 :: LocationCard LakeXochimilco_183
lakeXochimilco_183 = locationWith
  LakeXochimilco_183
  Cards.lakeXochimilco_183
  4
  (PerPlayer 2)
  (labelL .~ "heart")

instance HasModifiersFor LakeXochimilco_183 where
  getModifiersFor target (LakeXochimilco_183 a) | isTarget a target = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure []
      Just skillTest -> do
        sanityMatches <- fieldP
          InvestigatorRemainingSanity
          (<= 3)
          (skillTestInvestigator skillTest)
        let
          isInvestigate =
            skillTestAction skillTest == Just Action.Investigate && isTarget
              a
              (skillTestTarget skillTest)
        pure $ toModifiers
          a
          [ ShroudModifier (-2) | sanityMatches && isInvestigate ]
  getModifiersFor _ _ = pure []

instance HasAbilities LakeXochimilco_183 where
  getAbilities (LakeXochimilco_183 attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage LakeXochimilco_183 where
  runMessage msg l@(LakeXochimilco_183 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
      iids <- selectList $ investigatorAt (toId attrs)
      pushAll
        [ InvestigatorDirectDamage iid (toSource attrs) 0 1 | iid <- iids ]
      pure l
    _ -> LakeXochimilco_183 <$> runMessage msg attrs

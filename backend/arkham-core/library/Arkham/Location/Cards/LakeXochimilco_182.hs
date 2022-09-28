module Arkham.Location.Cards.LakeXochimilco_182
  ( lakeXochimilco_182
  , LakeXochimilco_182(..)
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

newtype LakeXochimilco_182 = LakeXochimilco_182 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lakeXochimilco_182 :: LocationCard LakeXochimilco_182
lakeXochimilco_182 = locationWith
  LakeXochimilco_182
  Cards.lakeXochimilco_182
  2
  (PerPlayer 1)
  (labelL .~ "heart")

instance HasModifiersFor LakeXochimilco_182 where
  getModifiersFor target (LakeXochimilco_182 a) | isTarget a target = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure []
      Just skillTest -> do
        actionsRemaining <- field
          InvestigatorRemainingActions
          (skillTestInvestigator skillTest)
        let
          isInvestigating =
            skillTestAction skillTest == Just Action.Investigate && isTarget
              a
              (skillTestTarget skillTest)
        pure $ toModifiers
          a
          [ ShroudModifier (2 * actionsRemaining)
          | actionsRemaining > 0 && isInvestigating
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities LakeXochimilco_182 where
  getAbilities (LakeXochimilco_182 attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage LakeXochimilco_182 where
  runMessage msg l@(LakeXochimilco_182 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
      iids <- selectList $ investigatorAt (toId attrs)
      pushAll [ SetActions iid (toSource attrs) 0 | iid <- iids ]
      pure l
    _ -> LakeXochimilco_182 <$> runMessage msg attrs

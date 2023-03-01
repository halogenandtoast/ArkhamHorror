module Arkham.Location.Cards.LakeXochimilco_182
  ( lakeXochimilco_182
  , LakeXochimilco_182(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
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
    miid <- getSkillTestInvestigator
    case miid of
      Nothing -> pure []
      Just iid -> do
        actionsRemaining <- field InvestigatorRemainingActions iid
        isBeingInvestigated <- getIsBeingInvestigated (toId a)
        pure $ toModifiers
          a
          [ ShroudModifier (2 * actionsRemaining)
          | actionsRemaining > 0 && isBeingInvestigated
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities LakeXochimilco_182 where
  getAbilities (LakeXochimilco_182 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (InvestigatorExists $ investigatorAt $ toId attrs)
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage LakeXochimilco_182 where
  runMessage msg l@(LakeXochimilco_182 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <- selectList $ investigatorAt (toId attrs)
      pushAll [ SetActions iid (toSource attrs) 0 | iid <- iids ]
      pure l
    _ -> LakeXochimilco_182 <$> runMessage msg attrs

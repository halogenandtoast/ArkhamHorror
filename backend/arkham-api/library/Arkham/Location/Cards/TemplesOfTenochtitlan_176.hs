module Arkham.Location.Cards.TemplesOfTenochtitlan_176 (
  templesOfTenochtitlan_176,
  TemplesOfTenochtitlan_176 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype TemplesOfTenochtitlan_176 = TemplesOfTenochtitlan_176 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_176 :: LocationCard TemplesOfTenochtitlan_176
templesOfTenochtitlan_176 =
  locationWith
    TemplesOfTenochtitlan_176
    Cards.templesOfTenochtitlan_176
    3
    (PerPlayer 1)
    (labelL .~ "square")

instance HasModifiersFor TemplesOfTenochtitlan_176 where
  getModifiersFor target (TemplesOfTenochtitlan_176 a) | isTarget a target = do
    miid <- getSkillTestInvestigator
    case miid of
      Nothing -> pure []
      Just iid -> do
        healthMatches <- fieldP InvestigatorRemainingHealth (<= 3) iid
        isBeingInvestigated <- getIsBeingInvestigated (toId a)
        pure
          $ toModifiers
            a
            [ShroudModifier (-2) | healthMatches && isBeingInvestigated]
  getModifiersFor _ _ = pure []

instance HasAbilities TemplesOfTenochtitlan_176 where
  getAbilities (TemplesOfTenochtitlan_176 attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ investigatorAt $ toId attrs)
          $ ForcedAbility
          $ PutLocationIntoPlay Timing.After Anyone
          $ LocationWithId
          $ toId attrs
      ]

instance RunMessage TemplesOfTenochtitlan_176 where
  runMessage msg l@(TemplesOfTenochtitlan_176 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      iids <- select $ investigatorAt (toId attrs)
      pushAll
        [InvestigatorDirectDamage iid (toSource attrs) 1 0 | iid <- iids]
      pure l
    _ -> TemplesOfTenochtitlan_176 <$> runMessage msg attrs

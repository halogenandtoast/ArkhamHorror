module Arkham.Asset.Cards.RelicOfAgesForestallingTheFuture (
  relicOfAgesForestallingTheFuture,
  RelicOfAgesForestallingTheFuture (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Metadata = Metadata {successTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RelicOfAgesForestallingTheFuture = RelicOfAgesForestallingTheFuture (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesForestallingTheFuture :: AssetCard RelicOfAgesForestallingTheFuture
relicOfAgesForestallingTheFuture =
  asset
    (RelicOfAgesForestallingTheFuture . (`with` Metadata False))
    Cards.relicOfAgesForestallingTheFuture

instance HasAbilities RelicOfAgesForestallingTheFuture where
  getAbilities (RelicOfAgesForestallingTheFuture (a `With` _)) =
    [restrictedAbility a 1 ControlsThis $ FastAbility $ exhaust a]

instance RunMessage RelicOfAgesForestallingTheFuture where
  runMessage msg a@(RelicOfAgesForestallingTheFuture (attrs `With` metadata)) =
    case msg of
      UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
        let
          chooseSkillTest skillType = beginSkillTest iid attrs iid skillType (Fixed 4)
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ SkillLabel skillType [chooseSkillTest skillType]
            | skillType <- [SkillWillpower, SkillIntellect]
            ]
        pure a
      PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ | not (successTriggered metadata) -> do
        agenda <- selectJust AnyAgenda
        push $ RemoveDoom (toAbilitySource attrs 1) (AgendaTarget agenda) 1
        pure . RelicOfAgesForestallingTheFuture $ attrs `with` Metadata True
      FailedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
        push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
        pure a
      _ -> RelicOfAgesForestallingTheFuture . (`with` metadata) <$> runMessage msg attrs

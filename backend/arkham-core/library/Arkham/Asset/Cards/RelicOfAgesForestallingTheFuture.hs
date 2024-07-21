module Arkham.Asset.Cards.RelicOfAgesForestallingTheFuture (
  relicOfAgesForestallingTheFuture,
  RelicOfAgesForestallingTheFuture (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
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
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        sid <- getRandom
        let chooseSkillTest skillType = beginSkillTest sid iid attrs iid skillType (Fixed 4)
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ SkillLabel skillType [chooseSkillTest skillType]
            | skillType <- [SkillWillpower, SkillIntellect]
            ]
        pure a
      PassedThisSkillTest _ (isSource attrs -> True) | not (successTriggered metadata) -> do
        agenda <- selectJust AnyAgenda
        push $ RemoveDoom (toAbilitySource attrs 1) (AgendaTarget agenda) 1
        pure . RelicOfAgesForestallingTheFuture $ attrs `with` Metadata True
      FailedThisSkillTest _ (isSource attrs -> True) -> do
        push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
        pure a
      _ -> RelicOfAgesForestallingTheFuture . (`with` metadata) <$> runMessage msg attrs

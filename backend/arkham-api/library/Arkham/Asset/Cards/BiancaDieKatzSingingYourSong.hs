module Arkham.Asset.Cards.BiancaDieKatzSingingYourSong (
  biancaDieKatzSingingYourSong,
  BiancaDieKatzSingingYourSong (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (searchBonded, withLocationOf)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Message (getChoiceAmount)
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Token

newtype Meta = Meta {movedResources :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BiancaDieKatzSingingYourSong = BiancaDieKatzSingingYourSong (AssetAttrs `With` Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

biancaDieKatzSingingYourSong :: AssetCard BiancaDieKatzSingingYourSong
biancaDieKatzSingingYourSong = ally (BiancaDieKatzSingingYourSong . (`with` Meta 0)) Cards.biancaDieKatzSingingYourSong (2, 1)

instance HasAbilities BiancaDieKatzSingingYourSong where
  getAbilities (BiancaDieKatzSingingYourSong (With x _)) =
    [skillTestAbility $ restrictedAbility x 1 ControlsThis parleyAction_]

instance RunMessage BiancaDieKatzSingingYourSong where
  runMessage msg a@(BiancaDieKatzSingingYourSong (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let resources = attrs.use Resource
      chooseAmount iid "Resources" "Resources" 0 (min 5 resources) attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      moveTokens (attrs.ability 1) attrs iid Resource n
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #agility (Fixed n)
      pure . BiancaDieKatzSingingYourSong $ attrs `with` Meta n
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      name <- field InvestigatorName iid
      remember $ YouOweBiancaResources (Labeled name iid) (movedResources meta)
      biancaDieKatz <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Enemies.biancaDieKatz
      withLocationOf iid $ createEnemyAt_ biancaDieKatz
      removeFromGame attrs
      pure a
    _ -> BiancaDieKatzSingingYourSong . (`with` meta) <$> liftRunMessage msg attrs

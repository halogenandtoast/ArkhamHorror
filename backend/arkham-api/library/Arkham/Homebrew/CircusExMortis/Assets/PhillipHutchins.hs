module Arkham.Homebrew.CircusExMortis.Assets.PhillipHutchins (phillipHutchins) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getViceCount)
import Arkham.Homebrew.CircusExMortis.Socialites
import Arkham.Message.Lifted.Choose
import Arkham.SkillType

newtype PhillipHutchins = PhillipHutchins AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

phillipHutchins :: AssetCard PhillipHutchins
phillipHutchins = asset PhillipHutchins Cards.phillipHutchins

instance HasAbilities PhillipHutchins where
  getAbilities (PhillipHutchins a) = socialiteAbilities a

instance RunMessage PhillipHutchins where
  runMessage msg a@(PhillipHutchins attrs) = runQueueT $ case msg of
    -- "Test any skill (2). This test gets +1 difficulty for each vice you have."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      vices <- getViceCount iid
      chooseSkillM iid allSkills \sType ->
        socialiteParley attrs iid sType 2 [Difficulty vices | vices > 0]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      socialiteSuccess attrs iid n
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      loseActions iid (attrs.ability 1) 1
      pure a
    _ -> PhillipHutchins <$> liftRunMessage msg attrs

module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.ProsperoPrinceGregariousHost (
  prosperoPrinceGregariousHost,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card (lookupCard, toCardId)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType (allSkills)
import Arkham.Trait (Trait (Cultist))

newtype ProsperoPrinceGregariousHost = ProsperoPrinceGregariousHost AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prosperoPrinceGregariousHost :: AssetCard ProsperoPrinceGregariousHost
prosperoPrinceGregariousHost = asset ProsperoPrinceGregariousHost Cards.prosperoPrinceGregariousHost

instance HasAbilities ProsperoPrinceGregariousHost where
  -- "[action]: Parley. Test any skill (2)."
  getAbilities (ProsperoPrinceGregariousHost a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage ProsperoPrinceGregariousHost where
  runMessage msg a@(ProsperoPrinceGregariousHost attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      -- "If you reveal a [skull], [cultist], [tablet], or [elder_thing] token,
      -- you must either immediately end your turn or place 1 doom on any other
      -- Cultist card."
      let tokens = oneOf [#skull, #cultist, #tablet, #elderthing]
      onRevealChaosTokenEffect sid tokens (attrs.ability 1) attrs do
        cultistAssets <- selectTargets $ AssetWithTrait Cultist <> not_ (be attrs)
        cultistEnemies <- selectTargets $ EnemyWithTrait Cultist
        chooseOneM iid do
          scenarioI18n
            $ scope "prosperoPrinceGregariousHost"
            $ labeled "endYourTurn"
            $ afterThisTestResolves sid
            $ endYourTurn iid
          targets (cultistAssets <> cultistEnemies) \target -> placeDoom (attrs.ability 1) target 1

      chooseSkillM iid allSkills \sType -> parley sid iid (attrs.ability 1) attrs sType (Fixed 2)
      pure a
    -- "If you succeed, remove a doom from Prospero Prince."
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      when (attrs.doom > 0) $ removeDoom (attrs.ability 1) attrs 1
      pure a
    -- Act 1b flips him too, but his back (023b) is an enemy rather than an asset:
    -- create it where he stands and let `Flipped` take the asset out of play.
    Flip _ _ (isTarget attrs -> True) -> do
      withLocationOf attrs \lid -> do
        let devotee = lookupCard Enemies.prosperoPrinceDevoteeOfKassogtha (toCardId attrs)
        createEnemyAt_ devotee lid
        push $ Flipped (toSource attrs) devotee
      pure a
    _ -> ProsperoPrinceGregariousHost <$> liftRunMessage msg attrs

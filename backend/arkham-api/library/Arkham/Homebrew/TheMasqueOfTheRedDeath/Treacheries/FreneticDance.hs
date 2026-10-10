module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.FreneticDance (freneticDance) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Import.Lifted

newtype FreneticDance = FreneticDance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

freneticDance :: TreacheryCard FreneticDance
freneticDance = treachery FreneticDance Cards.freneticDance

{- | "a [[Cultist]] scenario card at a location with an investigator"

The set's only [[Cultist]] scenario cards are its enemies and its [[Guest]] /
[[Victim]] story assets, so the doom lands on one of those two lists.
-}
cultistEnemies :: EnemyMatcher
cultistEnemies = EnemyWithTrait Cultist <> EnemyAt (LocationWithInvestigator Anyone)

cultistAssets :: AssetMatcher
cultistAssets = AssetWithTrait Cultist <> StoryAsset <> AssetAt (LocationWithInvestigator Anyone)

instance HasAbilities FreneticDance where
  getAbilities (FreneticDance a) =
    -- The round-end doom has to be limited: the window names no investigator, so
    -- every seat initiates the Forced ability and Frenetic Dance stays in play.
    [ groupLimit PerRound
        $ restricted a 1 (oneOf [exists cultistEnemies, exists cultistAssets])
        $ forced (RoundEnds #when)
    , -- "Investigators at any location may activate this ability": no location
      -- restriction, the card sits next to the agenda deck.
      skillTestAbility $ mkAbility a 2 parleyAction_
    ]

instance RunMessage FreneticDance where
  runMessage msg t@(FreneticDance attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      enemies <- selectTargets cultistEnemies
      assets <- selectTargets cultistAssets
      chooseOrRunOneM lead $ targets (enemies <> assets) \target -> placeDoom (attrs.ability 1) target 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      atBallroom <- iid <=~> InvestigatorAt (locationIs Locations.grandBallroom)
      when atBallroom $ skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      chooseSkillM iid [#willpower, #agility] \kind -> parley sid iid (attrs.ability 2) attrs kind (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> FreneticDance <$> liftRunMessage msg attrs

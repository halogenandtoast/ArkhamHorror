module Arkham.Asset.Cards.RichardUptonPickman (richardUptonPickman, RichardUptonPickman (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Modifiers hiding (skillTestModifiers)
import Arkham.Helpers.SkillTest
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Trait (Trait (Ghoul))

newtype RichardUptonPickman = RichardUptonPickman AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

richardUptonPickman :: AssetCard RichardUptonPickman
richardUptonPickman = assetWith RichardUptonPickman Cards.richardUptonPickman (healthL ?~ 3)

instance HasModifiersFor RichardUptonPickman where
  getModifiersFor (InvestigatorTarget _) (RichardUptonPickman a) = do
    toModifiers a . toList <$> runMaybeT do
      guardM $ isAbilitySource a 2 <$> MaybeT getSkillTestSource
      let n = count (`cardMatch` IsEncounterCard) $ assetCardsUnderneath a
      guard (n > 0) $> DamageDealt n
  getModifiersFor _ _ = pure []

instance HasAbilities RichardUptonPickman where
  getAbilities (RichardUptonPickman a) =
    [ controlledAbility a 1 (exists $ EnemyAt YourLocation <> withTrait Ghoul) $ FastAbility (exhaust a)
    , restrictedAbility a 2 ControlsThis $ fightAction (exhaust a)
    ]

instance RunMessage RichardUptonPickman where
  runMessage msg a@(RichardUptonPickman attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ghouls <- selectWithField EnemyCard (enemyAtLocationWith iid <> withTrait Ghoul)
      chooseOne
        iid
        [targetLabel ghoul [PlaceUnderneath (toTarget attrs) [card]] | (ghoul, card) <- ghouls]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 1
      skillTestModifiers source iid [BaseSkillOf #combat 5, NoStandardDamage]
      chooseFightEnemy iid (attrs.ability 1)
      pure a
    _ -> RichardUptonPickman <$> lift (runMessage msg attrs)

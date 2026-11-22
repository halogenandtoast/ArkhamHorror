module Arkham.Asset.Assets.KnightsLongsword (knightsLongsword) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype KnightsLongsword = KnightsLongsword AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightsLongsword :: AssetCard KnightsLongsword
knightsLongsword = asset KnightsLongsword Cards.knightsLongsword

instance HasAbilities KnightsLongsword where
  getAbilities (KnightsLongsword a) = [controlled_ a 1 fightAction_]

instance RunMessage KnightsLongsword where
  runMessage msg a@(KnightsLongsword attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 1) [DamageDealt 1]
      pure a
    Defeated (EnemyTarget _) _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> when attrs.ready do
        dreaming <-
          select $ DreamingCardControlledBy (InvestigatorWithId iid) <> DreamingCardWithDreams (atLeast 1)
        unless (null dreaming) do
          chooseOneM iid do
            targets dreaming \dc -> do
              exhaustThis attrs
              removeTokens (attrs.ability 1) dc Dream 1
            cardI18n $ labeled "knightsLongsword.doNotExhaust" nothing
      pure a
    _ -> KnightsLongsword <$> liftRunMessage msg attrs

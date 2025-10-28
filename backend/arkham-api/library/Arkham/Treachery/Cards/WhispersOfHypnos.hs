module Arkham.Treachery.Cards.WhispersOfHypnos (whispersOfHypnos, whispersOfHypnosEffect) where

import Arkham.Effect.Import
import Arkham.Effect.Types
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhispersOfHypnos = WhispersOfHypnos TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersOfHypnos :: TreacheryCard WhispersOfHypnos
whispersOfHypnos = treachery WhispersOfHypnos Cards.whispersOfHypnos

instance RunMessage WhispersOfHypnos where
  runMessage msg t@(WhispersOfHypnos attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      effects <- select $ effectFrom Cards.whispersOfHypnos
      usedSkills <- forMaybeM effects (fieldMap EffectMeta ((.skill) =<<))

      let skills = filter (`notElem` usedSkills) [#willpower, #intellect, #combat, #agility]
      chooseOrRunOneM iid do
        for_ skills \sType -> skillLabeled sType (forSkillType sType msg)
      pure t
    ForSkillType sType (Revelation _iid (isSource attrs -> True)) -> do
      createCardEffect Cards.whispersOfHypnos (Just $ EffectMetaSkill sType) attrs attrs
      pure t
    _ -> WhispersOfHypnos <$> liftRunMessage msg attrs

newtype WhispersOfHypnosEffect = WhispersOfHypnosEffect EffectAttrs
  deriving anyclass (IsEffect, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersOfHypnosEffect :: EffectArgs -> WhispersOfHypnosEffect
whispersOfHypnosEffect = cardEffect WhispersOfHypnosEffect Cards.whispersOfHypnos

instance HasModifiersFor WhispersOfHypnosEffect where
  getModifiersFor (WhispersOfHypnosEffect attrs) = for_ attrs.meta.skill \sType ->
    modifySelect attrs Anyone [SkillModifier sType (-2)]

instance RunMessage WhispersOfHypnosEffect where
  runMessage msg e@(WhispersOfHypnosEffect attrs) = runQueueT $ case msg of
    EndRound -> disableReturn e
    _ -> WhispersOfHypnosEffect <$> liftRunMessage msg attrs

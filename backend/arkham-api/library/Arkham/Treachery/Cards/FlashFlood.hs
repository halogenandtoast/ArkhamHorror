module Arkham.Treachery.Cards.FlashFlood (flashFlood) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FlashFlood = FlashFlood TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashFlood :: TreacheryCard FlashFlood
flashFlood = treachery FlashFlood Cards.flashFlood

instance HasModifiersFor FlashFlood where
  getModifiersFor (FlashFlood attrs) = case attrs.placement of
    AttachedToLocation lid -> modified_ attrs lid [ShroudModifier 4]
    _ -> pure mempty

instance HasAbilities FlashFlood where
  getAbilities (FlashFlood a) = 
    [ mkAbility a 1 $ forced $ RoundEnds #when
    ]

instance RunMessage FlashFlood where
  runMessage msg t@(FlashFlood attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> FlashFlood <$> liftRunMessage msg attrs

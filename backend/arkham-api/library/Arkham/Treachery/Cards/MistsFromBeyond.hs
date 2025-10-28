module Arkham.Treachery.Cards.MistsFromBeyond (mistsFromBeyond) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MistsFromBeyond = MistsFromBeyond TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsFromBeyond :: TreacheryCard MistsFromBeyond
mistsFromBeyond = treachery MistsFromBeyond Cards.mistsFromBeyond

instance HasModifiersFor MistsFromBeyond where
  getModifiersFor (MistsFromBeyond attrs) = for_ attrs.attached.location \loc ->
    modified_ attrs loc [ShroudModifier 1]

instance HasAbilities MistsFromBeyond where
  getAbilities (MistsFromBeyond a) =
    [ restricted
        a
        1
        (thisExists a (TreacheryAttachedToLocation LocationWithoutClues) <> exists LocationWithAnyClues)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage MistsFromBeyond where
  runMessage msg t@(MistsFromBeyond attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.location $ \loc -> do
        locations <- select $ NearestLocationToLocation loc LocationWithAnyClues
        chooseTargetM iid locations $ attachTreachery attrs
      pure t
    _ -> MistsFromBeyond <$> liftRunMessage msg attrs

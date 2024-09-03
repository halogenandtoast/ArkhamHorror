module Arkham.Treachery.Cards.RationalThought (
  rationalThought,
  RationalThought (..),
) where

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Damage
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window ()
import Arkham.Id
import Arkham.Placement
import Arkham.Projection
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window qualified as Window
import Data.UUID qualified as UUID

-- The metadata makes it so that the silent forced ability triggers only once
newtype Metadata = Metadata {discarding :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RationalThought = RationalThought (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rationalThought :: TreacheryCard RationalThought
rationalThought =
  treacheryWith (RationalThought . (`with` Metadata False)) Cards.rationalThought
    $ tokensL
    %~ addTokens Horror 4

-- NOTE: Preventing the resource gain is handled in Carolyn Fern. Ideally it
-- would be a modifier, but targetting such a specific interaction is
-- difficult. Since this is a signature, overlap like this is generally not a
-- concern, but we may want a more flexible solution in the future
instance HasModifiersFor RationalThought where
  getModifiersFor (InvestigatorTarget iid) (RationalThought (a `With` _)) = do
    horror <- fieldMap TreacheryTokens (countTokens Horror) a.id
    modified a
      $ guard (treacheryInThreatArea iid a)
      *> [CannotHealHorrorOnOtherCards (toTarget a), HealHorrorAsIfOnInvestigator (toTarget a) horror]
  getModifiersFor _ _ = pure []

-- Discard when no horror is on this
instance HasAbilities RationalThought where
  getAbilities (RationalThought (a `With` meta)) = case a.placement of
    InThreatArea _ -> [mkAbility a 1 Anytime | treacheryHorror a == 0 && not (discarding meta)]
    _ -> []

instance RunMessage RationalThought where
  runMessage msg t@(RationalThought (attrs `With` meta)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure $ RationalThought $ attrs `with` Metadata True
    HealHorror (isTarget attrs -> True) source amount -> do
      checkAfter $ Window.Healed HorrorType (toTarget attrs) source amount
      pure . RationalThought . (`with` meta) $ attrs & tokensL %~ subtractTokens Horror amount
    HealHorrorDirectly (InvestigatorTarget iid) _ amount
      | unCardCode (unInvestigatorId iid)
          == UUID.toText (unTreacheryId $ toId attrs) -> do
          -- USE ONLY WHEN NO CALLBACKS
          pure . RationalThought . (`with` meta) $ attrs & tokensL %~ subtractTokens Horror amount
    _ -> RationalThought . (`with` meta) <$> liftRunMessage msg attrs

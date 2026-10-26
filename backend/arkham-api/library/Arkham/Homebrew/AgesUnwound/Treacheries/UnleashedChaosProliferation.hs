module Arkham.Homebrew.AgesUnwound.Treacheries.UnleashedChaosProliferation (
  unleashedChaosProliferation,
) where

import Arkham.Card
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnleashedChaosProliferation = UnleashedChaosProliferation TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unleashedChaosProliferation :: TreacheryCard UnleashedChaosProliferation
unleashedChaosProliferation = treachery UnleashedChaosProliferation Cards.unleashedChaosProliferation

{- | "Revelation - Draw a card and gain a resource. Draw the top card of the
encounter deck. Until Unleashed Chaos (Proliferation) leaves play, treat it as an
exact copy of the drawn card and resolve it as if you had just drawn it."

The copy is THIS card, not a second one. An entity's behaviour is its Haskell
newtype, so this treachery cannot itself become the drawn card -- but the card
underneath it can, and that is what the printed text is about. So the drawn
card's identity is written onto Proliferation's own 'CardId', the Proliferation
entity is removed, and the card is drawn again under its new identity:

  * @ecCardCode@ becomes the drawn card's, so every def lookup, matcher and
    ability is the drawn card's -- "an exact copy".
  * @ecOriginalCardCode@ stays Proliferation's, which is both how the table can
    tell which of the two copies is this one and how the engine restores its
    real identity when it leaves play (@SetOriginalCardCode@, @Game/Runner@).
  * The 'CardId' is unchanged, so this is one physical card throughout.

'removeTreachery' rather than letting the revelation end normally: @Remove@ pops
the pending @After (Revelation ...)@ for that treachery (@Game/Runner@), which
is the message that would otherwise discard it out from under the copy. That pop
is why the card no longer lands in the encounter discard.

'ReplaceCard' and not the bare 'replaceCard': the registry mutation alone leaves
every zone holding the old card (@project_replacecard_registry_vs_message@).
-}
instance RunMessage UnleashedChaosProliferation where
  runMessage msg t@(UnleashedChaosProliferation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      drawCards iid attrs 1
      gainResources iid attrs 1
      mcard <- headMay <$> getEncounterDeck
      case mcard of
        -- Nothing to copy, so the revelation just ends and the card discards as
        -- usual -- deliberately no `removeTreachery` on this branch.
        Nothing -> pure ()
        Just card -> do
          drawEncounterCard iid attrs
          let
            copy =
              card
                { ecId = toCardId attrs
                , ecOriginalCardCode = toCardCode Cards.unleashedChaosProliferation
                }
          push $ ReplaceCard (toCardId attrs) (EncounterCard copy)
          removeTreachery attrs
          push $ InvestigatorDrewEncounterCard iid copy
      pure t
    _ -> UnleashedChaosProliferation <$> liftRunMessage msg attrs

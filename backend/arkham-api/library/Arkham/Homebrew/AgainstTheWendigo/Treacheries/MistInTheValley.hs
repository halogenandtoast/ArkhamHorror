module Arkham.Homebrew.AgainstTheWendigo.Treacheries.MistInTheValley (mistInTheValley) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (isSymbolFace)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype MistInTheValley = MistInTheValley TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistInTheValley :: TreacheryCard MistInTheValley
mistInTheValley = treachery MistInTheValley Cards.mistInTheValley

instance HasModifiersFor MistInTheValley where
  -- "Attached location gets +2 shroud."
  getModifiersFor (MistInTheValley a) =
    for_ a.attached.location \lid -> modified_ a lid [ShroudModifier 2]

instance HasAbilities MistInTheValley where
  getAbilities (MistInTheValley a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage MistInTheValley where
  runMessage msg t@(MistInTheValley attrs) = runQueueT $ case msg of
    -- "Attach to your location. Limit 1 per location."
    Revelation iid (isSource attrs -> True) -> do
      already <- selectAny $ LocationWithTreachery (treacheryIs Cards.mistInTheValley) <> locationWithInvestigator iid
      if already
        then toDiscard attrs attrs
        else withLocationOf iid $ attachTreachery attrs
      pure t
    {- | "At the end of the round, reveal a chaos token: if a symbol is revealed,
    attach this card to a location directly to the East or West. Otherwise
    discard this card." The valley's compass connections are its grid
    connections, so the east/west neighbours are the connected locations. -}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      requestChaosTokens lead (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      if any isSymbolFace tokens
        then do
          neighbours <- case a.attached.location of
            Nothing -> pure []
            Just lid -> select $ connectedFrom (LocationWithId lid)
          chooseOrRunOneM iid $ targets neighbours $ attachTreachery attrs
        else toDiscard (attrs.ability 1) attrs
      pure t
     where
      a = attrs
    _ -> MistInTheValley <$> liftRunMessage msg attrs

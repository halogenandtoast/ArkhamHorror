module Arkham.Homebrew.DarkMatter.Agendas.AgainstTheSun (againstTheSun) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card (toCard)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Trait (Trait (Byakhee))

newtype AgainstTheSun = AgainstTheSun AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

againstTheSun :: AgendaCard AgainstTheSun
againstTheSun = agenda (4, A) AgainstTheSun Cards.againstTheSun (Static 3)

{- | "Forced - When a [[Brain]] story asset is defeated: Remove it from the game
and add 1 tally mark under 'Impending Doom' in your Campaign Log."
-}
instance HasAbilities AgainstTheSun where
  getAbilities (AgainstTheSun a) =
    [mkAbility a 1 $ forced $ Matcher.AssetDefeated #when ByAny (AssetWithTrait Brain)]

instance RunMessage AgainstTheSun where
  runMessage msg a@(AgainstTheSun attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDefeatedAsset -> aid) _ -> do
      push $ RemoveFromGame (AssetTarget aid)
      addImpendingDoom 1
      pure a
    {- Agenda 4b, "They are Everywhere!":

    "Shuffle the encounter discard pile into the encounter deck.
    Discard cards from the top of the encounter deck until 1[per_investigator]
    Byakhee enemies are discarded. Spawn them all at the Entrance Tunnel.
    Flip this agenda back to agenda 4a."

    The scenario never ends on this agenda; it keeps cycling. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      lead <- getLead
      n <- perPlayer 1
      discardUntilN n lead attrs attrs Deck.EncounterDeck $ basic (#enemy <> withTrait Byakhee)
      revertAgenda attrs
      pure a
    RequestedEncounterCards (isTarget attrs -> True) cards -> do
      entranceTunnel <- selectJust $ locationIs Locations.entranceTunnel
      for_ cards \card -> createEnemyAt_ (toCard card) entranceTunnel
      pure a
    _ -> AgainstTheSun <$> liftRunMessage msg attrs

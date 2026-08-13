module Arkham.Homebrew.DarkMatter.Agendas.FlightOfTheByakhees (flightOfTheByakhees) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Matcher qualified as Matcher

newtype FlightOfTheByakhees = FlightOfTheByakhees AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flightOfTheByakhees :: AgendaCard FlightOfTheByakhees
flightOfTheByakhees = agenda (3, A) FlightOfTheByakhees Cards.flightOfTheByakhees (Static 5)

{- | "Forced - When a [[Brain]] story asset is defeated: Remove it from the game
and add 1 tally mark under 'Impending Doom' in your Campaign Log."
-}
instance HasAbilities FlightOfTheByakhees where
  getAbilities (FlightOfTheByakhees a) =
    [mkAbility a 1 $ forced $ Matcher.AssetDefeated #when ByAny (AssetWithTrait Brain)]

instance RunMessage FlightOfTheByakhees where
  runMessage msg a@(FlightOfTheByakhees attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDefeatedAsset -> aid) _ -> do
      push $ RemoveFromGame (AssetTarget aid)
      addImpendingDoom 1
      pure a
    {- This agenda has no 3b: the back of the card is the Haïta enemy
    (@:dark-matter:156b@, linked by 'otherSideIs' \/ 'doubleSided' in the card
    defs). Advancing therefore flips the card into Haïta, who spawns at her
    printed "Spawn - Entrance Tunnel", and the agenda deck moves on to agenda 4.

    Same shape as The Ghost Ship (@:dark-matter:016@), whose back is the
    UPL-A21 "Demhe" enemy. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      entranceTunnel <- selectJust $ locationIs Locations.entranceTunnel
      createEnemyAt_ Enemies.haita entranceTunnel
      advanceAgendaDeck attrs
      pure a
    _ -> FlightOfTheByakhees <$> liftRunMessage msg attrs

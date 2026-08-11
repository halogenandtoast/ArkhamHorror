module Arkham.Homebrew.DarkMatter.Agendas.NostalgiaAgendas (
  theNostalgiaII,
  theThingFromEarth,
  screamOfTheDead,
  itsWeirdAndPissedOff,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (getScanningDeck, scanAction_, scanIcons)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Crew))

{- | All four "In the Shadow of Earth" agendas share their front text:

"[action] Scan. If the top card of the scanning deck has an icon matching your
current location, spend 1[per_investigator] clues, as a group: Draw the top card
of the scanning deck.
Forced - When a [[Crew]] story asset is defeated or discarded: Remove it from the
game."

Agenda 4 replaces the Forced clause: the crew asset is attached face down to The
Entity instead of being removed.

TODO(homebrew): the group clue cost is not charged. The printed ability checks
the (face-down) top card *before* payment, so the cost cannot be declared on the
ability — and declaring it anyway would spend clues even when the icon does not
match, which is worse than undercharging. Charging it needs an imperative
group-clue payment after the peek.
-}
newtype NostalgiaAgenda = NostalgiaAgenda AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkNostalgia :: Int -> CardDef -> GameValue -> AgendaCard NostalgiaAgenda
mkNostalgia n def = agenda (n, A) NostalgiaAgenda def

theNostalgiaII :: AgendaCard NostalgiaAgenda
theNostalgiaII = mkNostalgia 1 Cards.theNostalgiaII (Static 3)

theThingFromEarth :: AgendaCard NostalgiaAgenda
theThingFromEarth = mkNostalgia 2 Cards.theThingFromEarth (Static 4)

screamOfTheDead :: AgendaCard NostalgiaAgenda
screamOfTheDead = mkNostalgia 3 Cards.screamOfTheDead (Static 3)

itsWeirdAndPissedOff :: AgendaCard NostalgiaAgenda
itsWeirdAndPissedOff = mkNostalgia 4 Cards.itsWeirdAndPissedOff (Static 2)

-- | Only the final agenda attaches defeated crew to The Entity.
isFinalAgenda :: AgendaAttrs -> Bool
isFinalAgenda a = toCardCode (toCardDef a) == toCardCode Cards.itsWeirdAndPissedOff

instance HasAbilities NostalgiaAgenda where
  getAbilities (NostalgiaAgenda a) =
    [ restricted a 1 NoRestriction scanAction_
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (AssetWithTrait Crew)
    ]

instance RunMessage NostalgiaAgenda where
  runMessage msg a@(NostalgiaAgenda attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      symbol <- field LocationPrintedSymbol lid
      deck <- getScanningDeck
      for_ (take 1 deck) \card ->
        when (symbol `elem` scanIcons card) $ drawCard iid card
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      crew <- select $ AssetWithTrait Crew
      for_ crew \aid ->
        if isFinalAgenda attrs
          then
            selectOne (enemyIs Enemies.theEntity) >>= traverse_ \entity ->
              push $ PlaceAsset aid (AttachedToEnemy entity)
          else push $ RemoveFromGame (AssetTarget aid)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> NostalgiaAgenda <$> liftRunMessage msg attrs

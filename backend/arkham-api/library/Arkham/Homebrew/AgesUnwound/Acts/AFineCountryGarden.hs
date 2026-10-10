module Arkham.Homebrew.AgesUnwound.Acts.AFineCountryGarden (aFineCountryGarden) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (remembered)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.Matcher

newtype AFineCountryGarden = AFineCountryGarden ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aFineCountryGarden :: ActCard AFineCountryGarden
aFineCountryGarden = act (1, A) AFineCountryGarden Cards.aFineCountryGarden Nothing

{- | "Objective - At the end of the round, the investigators may spend the
requisite number of clues, as a group, to advance."
-}
instance HasAbilities AFineCountryGarden where
  getAbilities (AFineCountryGarden x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 3) Anywhere
    ]

instance RunMessage AFineCountryGarden where
  runMessage msg a@(AFineCountryGarden attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      {- "Place the set-aside The Myriad Gentleman (Thousandfold Man) next to the
      agenda. This card is not in play." It is a display card the copies are made
      from, so it stays a Card rather than becoming an enemy entity. 'obtainCard'
      first, or the set-aside pool keeps its own copy. -}
      displayed <- getSetAsideCard Enemies.theMyriadGentleman_042
      obtainCard displayed
      push $ PlaceNextTo AgendaDeckTarget [displayed]

      -- "Each investigator spawns a copy of The Myriad Gentleman, engaged with them."
      eachInvestigator (`spawnMyriadCopiesEngagedWith` 1)

      {- "If "he is ready for you", the lead investigator spawns 1[per_investigator]
      copies of The Myriad Gentleman engaged with them." -}
      whenM (remembered heIsReadyForYou) do
        n <- perPlayer 1
        lead <- getLead
        spawnMyriadCopiesEngagedWith lead n

      -- "Put each set-aside location into play."
      placeSetAsideLocationsMatching_ AnyCard

      {- "Shuffle each set-aside [[Myriad]] treachery into the encounter deck,
      along with the encounter discard pile." -}
      shuffleSetAsideIntoEncounterDeck (CardWithType TreacheryType <> CardWithTrait Myriad)
      shuffleEncounterDiscardBackIn

      advanceActDeck attrs
      pure a
    _ -> AFineCountryGarden <$> liftRunMessage msg attrs

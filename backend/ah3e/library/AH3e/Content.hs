module AH3e.Content (
  cardDefs,
  cardDef,
  investigatorDefs,
  investigatorDef,
  scenarioDefs,
  scenarioDef,
  ScenarioInfo (..),
  scenarioCatalog,
  focusLimitBonus,
  focusLimitFromSpells,
  focusLimitFromAllies,
  spendableMoneyCards,
  encounterPhaseSkillBonus,
  sharesFocusedSkills,
) where

import AH3e.Content.Allies qualified as Allies
import AH3e.Content.Conditions qualified as Conditions
import AH3e.Content.Core.ApproachOfAzathoth qualified as ApproachOfAzathoth
import AH3e.Content.Core.EchoesOfTheDeep qualified as EchoesOfTheDeep
import AH3e.Content.Core.FeastOfUmordhoth qualified as FeastOfUmordhoth
import AH3e.Content.Core.Investigators qualified as Investigators
import AH3e.Content.Core.NeighborhoodCards qualified as NeighborhoodCards
import AH3e.Content.Core.VeilOfTwilight qualified as VeilOfTwilight
import AH3e.Content.DeadOfNight.Encounters qualified as DeadOfNightEncounters
import AH3e.Content.DeadOfNight.Investigators qualified as DeadOfNightInvestigators
import AH3e.Content.DeadOfNight.ShotsInTheDark qualified as ShotsInTheDark
import AH3e.Content.DeadOfNight.SilenceOfTsathoggua qualified as SilenceOfTsathoggua
import AH3e.Content.Headlines qualified as Headlines
import AH3e.Content.Items qualified as Items
import AH3e.Content.Monsters qualified as Monsters
import AH3e.Content.Scenarios
import AH3e.Content.SecretsOfTheOrder.Anomalies qualified as SecretsOfTheOrderAnomalies
import AH3e.Content.SecretsOfTheOrder.Archive qualified as SecretsOfTheOrderArchive
import AH3e.Content.SecretsOfTheOrder.BoundToServe qualified as BoundToServe
import AH3e.Content.SecretsOfTheOrder.Investigators qualified as SecretsOfTheOrderInvestigators
import AH3e.Content.SecretsOfTheOrder.Mysteries qualified as SecretsOfTheOrderMysteries
import AH3e.Content.SecretsOfTheOrder.NeighborhoodCards qualified as SecretsOfTheOrderNeighborhoodCards
import AH3e.Content.SecretsOfTheOrder.TheDeadCryOut qualified as TheDeadCryOut
import AH3e.Content.SecretsOfTheOrder.TheKeyAndTheGate qualified as TheKeyAndTheGate
import AH3e.Content.SecretsOfTheOrder.Thresholds qualified as SecretsOfTheOrderThresholds
import AH3e.Content.Special qualified as Special
import AH3e.Content.Spells qualified as Spells
import AH3e.Content.StreetCards qualified as StreetCards
import AH3e.Content.UnderDarkWaves.Anomalies qualified as UnderDarkWavesAnomalies
import AH3e.Content.UnderDarkWaves.Archive qualified as UnderDarkWavesArchive
import AH3e.Content.UnderDarkWaves.DreamsOfRlyeh qualified as DreamsOfRlyeh
import AH3e.Content.UnderDarkWaves.Investigators qualified as UnderDarkWavesInvestigators
import AH3e.Content.UnderDarkWaves.IthaquasChildren qualified as IthaquasChildren
import AH3e.Content.UnderDarkWaves.Mysteries qualified as UnderDarkWavesMysteries
import AH3e.Content.UnderDarkWaves.NeighborhoodCards qualified as UnderDarkWavesNeighborhoodCards
import AH3e.Content.UnderDarkWaves.Terrors qualified as UnderDarkWavesTerrors
import AH3e.Content.UnderDarkWaves.ThePaleLantern qualified as ThePaleLantern
import AH3e.Content.UnderDarkWaves.TravelRoutes qualified as UnderDarkWavesTravelRoutes
import AH3e.Content.UnderDarkWaves.TyrantsOfRuin qualified as TyrantsOfRuin
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids
import Data.Map.Strict qualified as Map

{- | How much holding this card raises its holder's focus limit.
| Sheets whose focus limit is counted from what they hold.
-}
focusLimitFromSpells :: [InvestigatorId]
focusLimitFromSpells = Investigators.focusLimitFromSpells

-- | Sheets whose focus limit is counted from the allies they hold instead.
focusLimitFromAllies :: [InvestigatorId]
focusLimitFromAllies = UnderDarkWavesInvestigators.focusLimitFromAllies

-- | Cards whose money their holder may spend as their own.
spendableMoneyCards :: [CardCode]
spendableMoneyCards = UnderDarkWavesInvestigators.spendableMoneyCards

-- | How much this card adds to every skill its holder has during the encounter phase.
encounterPhaseSkillBonus :: CardCode -> Int
encounterPhaseSkillBonus code =
  Map.findWithDefault 0 code (Map.fromList UnderDarkWavesInvestigators.encounterPhaseSkillBonuses)

-- | Cards that share their holder's focused skills with their space.
sharesFocusedSkills :: [CardCode]
sharesFocusedSkills = Investigators.sharesFocusedSkills

focusLimitBonus :: CardCode -> Int
focusLimitBonus code =
  Map.findWithDefault
    0
    code
    (Map.fromList (Special.focusLimitBonuses <> Conditions.focusLimitBonuses))

cardDefs :: Map CardCode CardDef
cardDefs =
  Map.fromList
    [ (d.code, d)
    | d <-
        ApproachOfAzathoth.cards
          <> EchoesOfTheDeep.cards
          <> FeastOfUmordhoth.cards
          <> VeilOfTwilight.cards
          <> SilenceOfTsathoggua.cards
          <> ShotsInTheDark.cards
          <> DeadOfNightInvestigators.cards
          <> UnderDarkWavesInvestigators.cards
          <> SecretsOfTheOrderInvestigators.cards
          <> DeadOfNightEncounters.cards
          <> Investigators.cards
          <> NeighborhoodCards.cards
          <> UnderDarkWavesNeighborhoodCards.cards
          <> SecretsOfTheOrderNeighborhoodCards.cards
          <> SecretsOfTheOrderThresholds.cards
          <> SecretsOfTheOrderMysteries.cards
          <> SecretsOfTheOrderAnomalies.cards
          <> SecretsOfTheOrderArchive.cards
          <> BoundToServe.cards
          <> TheKeyAndTheGate.cards
          <> TheDeadCryOut.cards
          <> UnderDarkWavesMysteries.cards
          <> UnderDarkWavesTravelRoutes.cards
          <> UnderDarkWavesAnomalies.cards
          <> UnderDarkWavesTerrors.cards
          <> UnderDarkWavesArchive.cards
          <> TyrantsOfRuin.cards
          <> ThePaleLantern.cards
          <> IthaquasChildren.cards
          <> DreamsOfRlyeh.cards
          <> StreetCards.cards
          <> Items.cards
          <> Spells.cards
          <> Allies.cards
          <> Special.cards
          <> Monsters.cards
          <> Headlines.cards
          <> Conditions.cards
    ]

cardDef :: CardCode -> Maybe CardDef
cardDef code = Map.lookup code cardDefs

investigatorDefs :: Map InvestigatorId InvestigatorDef
investigatorDefs =
  Map.fromList
    [ (d.id, d)
    | d <-
        Investigators.investigators
          <> DeadOfNightInvestigators.investigators
          <> UnderDarkWavesInvestigators.investigators
          <> SecretsOfTheOrderInvestigators.investigators
    ]

investigatorDef :: InvestigatorId -> Maybe InvestigatorDef
investigatorDef iid = Map.lookup iid investigatorDefs

scenarioDefs :: Map ScenarioCode ScenarioDef
scenarioDefs =
  Map.fromList
    [ (d.code, d)
    | d <-
        [ ApproachOfAzathoth.scenario
        , EchoesOfTheDeep.scenario
        , FeastOfUmordhoth.scenario
        , VeilOfTwilight.scenario
        , SilenceOfTsathoggua.scenario
        , ShotsInTheDark.scenario
        , TyrantsOfRuin.scenario
        , ThePaleLantern.scenario
        , DreamsOfRlyeh.scenario
        , IthaquasChildren.scenario
        , BoundToServe.scenario
        , TheKeyAndTheGate.scenario
        , TheDeadCryOut.scenario
        ]
    ]

scenarioDef :: ScenarioCode -> Maybe ScenarioDef
scenarioDef code = Map.lookup code scenarioDefs

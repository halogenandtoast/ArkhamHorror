module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.ThroughTheLabyrinthV2 (throughTheLabyrinthV2) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator, scenarioI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers hiding (scenarioI18n)
import Arkham.Trait (Trait (DeepOne))

newtype ThroughTheLabyrinthV2 = ThroughTheLabyrinthV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Who resigned while they were a Deep One.

The trait comes from a card in play -- Stalked by Deep Ones in a threat area, Innsmouth
Influence in a deck -- and resigning discards everything the investigator held, so the
trait is gone by the time this act's back is read. The act writes down who had it as they
resign and hands the trait back for the rest of the scenario.

It has to be written into the act rather than queued: the last investigator to resign
runs @HandleNoRemainingInvestigators@, which clears the queue, so a modifier pushed from
the resignation never resolves.
-}
instance HasModifiersFor ThroughTheLabyrinthV2 where
  getModifiersFor (ThroughTheLabyrinthV2 a) = case resignedAsDeepOnes a of
    [] -> pure ()
    iids -> modifySelect a (IncludeEliminated $ oneOf (map InvestigatorWithId iids)) [AddTrait DeepOne]

resignedAsDeepOnes :: ActAttrs -> [InvestigatorId]
resignedAsDeepOnes a = toResultDefault [] a.meta

throughTheLabyrinthV2 :: ActCard ThroughTheLabyrinthV2
throughTheLabyrinthV2 = act (1, A) ThroughTheLabyrinthV2 Cards.throughTheLabyrinthV2 Nothing

instance HasAbilities ThroughTheLabyrinthV2 where
  getAbilities (ThroughTheLabyrinthV2 a) =
    extend
      a
      [ restricted a 1 (exists $ YourLocation <> LocationWithAdjacentBarrier)
          $ FastAbility (GroupClueCost (StaticWithPerPlayer 1 1) Anywhere)
      , onlyOnce $ restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

{- | Differs from Through the Labyrinth (07128) only on the back: a Deep One investigator
leaves town carrying Innsmouth Influence with them.
-}
instance RunMessage ThroughTheLabyrinthV2 where
  runMessage msg a@(ThroughTheLabyrinthV2 attrs) = runQueueT $ scenarioI18n "returnToInTooDeep" $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid (removeBarrierBetweenConnected iid)
      pure a
    Resign iid -> do
      isDeepOne <- iid <=~> deepOneInvestigator
      pure
        $ if isDeepOne
          then ThroughTheLabyrinthV2 $ attrs & metaL .~ toJSON (iid : resignedAsDeepOnes attrs)
          else a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record TheInvestigatorsMadeItSafelyToTheirVehicles
      selectEach (IncludeEliminated deepOneInvestigator) \iid ->
        addCampaignCardToDeck iid ShuffleIn HBTreacheries.innsmouthInfluence
      push R1
      pure a
    _ -> ThroughTheLabyrinthV2 <$> liftRunMessage msg attrs

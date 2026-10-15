{- | Agenda 2, Rushing Asunder.

It prints no doom threshold: the first __Forced__ strips every doom that lands
on it and spends it as damage on the house instead, so the agenda can only
advance through its third __Forced__, once the last [[Decadent]] room has
collapsed.
-}
module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Agendas.RushingAsunder (rushingAsunder) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Doom (getDoomOnTarget)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement (Placement (InThreatArea))

newtype RushingAsunder = RushingAsunder AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rushingAsunder :: AgendaCard RushingAsunder
rushingAsunder =
  agendaWith (2, A) RushingAsunder Cards.rushingAsunder (Static 0) (doomThresholdL .~ Nothing)

anyDoomInPlay :: Criterion
anyDoomInPlay =
  AnyCriterion
    [ exists EnemyWithAnyDoom
    , exists TreacheryWithAnyDoom
    , exists AssetWithAnyDoom
    , exists EventWithAnyDoom
    , exists InvestigatorWithAnyDoom
    ]

instance HasAbilities RushingAsunder where
  getAbilities (RushingAsunder a) =
    [ mkAbility a 1 $ forced $ PlacedDoomCounter #after AnySource (targetIs a)
    , restricted a 2 anyDoomInPlay $ forced $ PhaseBegins #when #mythos
    , onlyOnce $ restricted a 3 (notExists decadentLocation) $ forced AnyWindow
    ]

instance RunMessage RushingAsunder where
  runMessage msg a@(RushingAsunder attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Nothing else ever rests here, so every doom on the agenda is "that doom".
      removeAllDoom (attrs.ability 1) attrs
      doStep 2 msg
      pure a
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      rooms <- decadentRoomsWithCapacity
      unless (null rooms) $ chooseTargetM iid rooms \lid -> do
        placeTokens (attrs.ability 1) lid #damage 1
        doNextStep msg
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      iids <- select UneliminatedInvestigator
      owned <- concatForM iids \iid -> do
        assets <- selectTargets $ AssetWithAnyDoom <> AssetControlledBy (InvestigatorWithId iid)
        events <- selectTargets $ EventWithAnyDoom <> EventControlledBy (InvestigatorWithId iid)
        treacheries <-
          selectTargets $ TreacheryWithAnyDoom <> TreacheryInThreatAreaOf (InvestigatorWithId iid)
        enemies <- selectTargets $ EnemyWithAnyDoom <> EnemyWithPlacement (InThreatArea iid)
        self <- selectTargets $ InvestigatorWithAnyDoom <> InvestigatorWithId iid
        pure $ map (iid,) (assets <> events <> treacheries <> enemies <> self)

      doomedEnemies <- selectTargets EnemyWithAnyDoom
      doomedTreacheries <- selectTargets TreacheryWithAnyDoom
      let swept = filter (`notElem` map snd owned) (doomedEnemies <> doomedTreacheries)
      total <- sum <$> traverse getDoomOnTarget swept
      for_ swept \target -> removeDoom (attrs.ability 2) target =<< getDoomOnTarget target
      placeDoom (attrs.ability 2) attrs total

      for_ owned \(iid, target) -> do
        n <- getDoomOnTarget target
        replicateM_ n $ chooseOneM iid $ scope "rushingAsunder" do
          labeled "moveDoomToAgenda" do
            removeDoom (attrs.ability 2) target 1
            placeDoom (attrs.ability 2) attrs 1
          unscoped do
            countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 2) 1
            countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 2) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach UneliminatedInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> RushingAsunder <$> liftRunMessage msg attrs

{- | Act 1, Cause U. N. Owen.

Act 1a buys evidence: each use turns one facedown [[Method]] from under a
location face up on a [[Victim]] standing there, and the icons the players see
are all they ever learn before the objective asks them to name a killer.

Act 1b then reveals every [[Method]] that was never bought -- attaching those to
the act, where each one costs a point of doom if the accusation was wrong --
tallies all ten and convicts the victim whose own icons match the fewest. The
accusation itself is carried across the advance in this act's meta, as the named
victim's card code, because the flip takes the victim's asset side out of play.

The sigils are hung one location at a time, through 'HandleTargetChoice': each
copy has to be fetched from the set-aside pile after the previous one has left
it, which a single handler building four placements at once cannot do.
-}
module Arkham.Homebrew.AndThenThereWereNone.Acts.CauseUNOwen (causeUNOwen) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Homebrew.AndThenThereWereNone.Traits (pattern Method)
import Arkham.I18n
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype CauseUNOwen = CauseUNOwen ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

causeUNOwen :: ActCard CauseUNOwen
causeUNOwen = act (1, A) CauseUNOwen Cards.causeUNOwen Nothing

-- | A facedown murder weapon, which is a card beneath a location rather than an asset.
facedownMethod :: LocationMatcher
facedownMethod = LocationWithCardsUnderneath $ HasCard $ CardWithTrait Method

instance HasAbilities CauseUNOwen where
  getAbilities = actAbilities \a ->
    scenarioI18n
      [ -- "{fast} Investigators at your location spend 1{perPlayer} clues as a
        -- group: Reveal a facedown encounter card beneath your location and
        -- attach it to a Victim card at that location without a Method card
        -- attached."
        withI18nTooltip "causeUNOwen.reveal"
          $ restricted
            a
            1
            ( exists (YourLocation <> facedownMethod)
                <> exists (victimWithoutMethod <> AssetAt YourLocation)
            )
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) YourLocation
      , -- "Objective - At the end of the round, the investigators may name the
        -- killer and advance."
        withI18nTooltip "causeUNOwen.objective"
          $ restricted a 2 (exists victimAsset)
          $ Objective
          $ freeReaction (RoundEnds #when)
      ]

instance RunMessage CauseUNOwen where
  runMessage msg a@(CauseUNOwen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        cards <- fieldMap LocationCardsUnderneath (filterCards $ CardWithTrait Method) lid
        victims <- select $ victimWithoutMethod <> AssetAt (LocationWithId lid)
        -- The weapons were shuffled face down at setup, so the one on top is
        -- as unknown to the players as any other.
        unless (null victims) $ for_ (take 1 cards) \card -> do
          removeFromUnderneath lid [card]
          focusCards [card] $ chooseOneM iid $ scenarioI18n $ scope "causeUNOwen" do
            questionLabeled "attachMethod"
            targets victims \vid -> createAssetAt_ card (AttachedToAsset vid Nothing)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lead <- getLead
      victims <- select victimAsset
      scenarioI18n $ scope "causeUNOwen" $ chooseOneM lead do
        questionLabeled "nameTheKiller"
        targets victims $ handleTarget lead attrs
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (AssetTarget vid) -> do
      card <- field AssetCard vid
      advancedWithOther attrs
      pure . CauseUNOwen $ attrs & setMeta (Just $ toCardCode card)
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Reveal each encounter card facedown beneath each location and attach
      -- them to this act."
      selectEach facedownMethod \lid -> do
        cards <- fieldMap LocationCardsUnderneath (filterCards $ CardWithTrait Method) lid
        removeFromUnderneath lid cards
        for_ cards \card -> createAssetAt_ card (AttachedToAct attrs.id)
      -- The tally reads the Methods in play, so it waits for those to arrive.
      doStep 1 msg
      pure a
    DoStep 1 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      killers <- trueKiller
      unless (null killers) do
        chooseOrRunOneM lead $ targets killers \vid -> do
          -- "Flip that story asset to its enemy side." Each Victim owns its own back.
          flipOverBy lead attrs vid
          card <- field AssetCard vid
          -- "If the investigators did not guess correctly, place 2 doom on the
          -- current agenda, plus 1 doom for each card attached to this act."
          unless (namedKiller attrs.meta == Just (toCardCode card)) do
            attached <- selectCount $ methodAsset <> AssetAttachedTo (TargetIs $ toTarget attrs)
            placeDoomOnAgenda (2 + attached)
      doStep 2 msg'
      pure a
    DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      -- "Attach one copy of Antediluvian Sigils to the 1{perPlayer} nearest
      -- locations to the Culprit enemy (including its location)."
      lead <- getLead
      n <- perPlayer 1
      selectOne culpritEnemy >>= traverse_ \culprit -> withLocationOf culprit \lid -> do
        ranked <- locationsByDistanceFrom lid Anywhere
        for_ (lastMay $ take n ranked) \(cutoff, _) -> do
          let nearer = [l | (d, l) <- ranked, d < cutoff]
          let tied = [l | (d, l) <- ranked, d == cutoff]
          let remaining = n - length nearer
          for_ nearer $ handleTarget lead attrs
          if remaining >= length tied
            then for_ tied $ handleTarget lead attrs
            else scenarioI18n $ scope "causeUNOwen" $ chooseNM lead remaining do
              questionLabeled "sigilLocation"
              targets tied $ handleTarget lead attrs
      doStep 3 msg'
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (LocationTarget lid) -> do
      copies <- getSetAsideCardsMatching (cardIs Assets.antediluvianSigils)
      for_ (take 1 copies) \card -> createAssetAt_ card (AttachedToLocation lid)
      pure a
    DoStep 3 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      -- "Remove each other copy from the game."
      traverse_ removeCardFromGame =<< getSetAsideCardsMatching (cardIs Assets.antediluvianSigils)
      -- "Each investigator loses all of their clues."
      eachInvestigator $ removeAllClues attrs
      -- "Replenish clues on each revealed location up to its starting clue value."
      selectEach (#revealed <> LocationNotAtClueLimit) $ placeCluesUpToClueValue attrs
      advanceActDeck attrs
      pure a
    _ -> CauseUNOwen <$> liftRunMessage msg attrs

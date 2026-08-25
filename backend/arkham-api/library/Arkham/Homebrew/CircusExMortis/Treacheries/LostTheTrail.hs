module Arkham.Homebrew.CircusExMortis.Treacheries.LostTheTrail (lostTheTrail) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Movement (replaceMovement)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Movement (Destination (ToLocation), Movement (moveDestination))
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype LostTheTrail = LostTheTrail TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostTheTrail :: TreacheryCard LostTheTrail
lostTheTrail = treachery LostTheTrail Cards.lostTheTrail

goesAstray :: [ChaosTokenFace]
goesAstray = [Skull, Cultist, Tablet, ElderThing, AutoFail, MoonToken]

instance HasModifiersFor LostTheTrail where
  getModifiersFor (LostTheTrail a) = case a.placement of
    InThreatArea iid -> do
      modified_ a iid [AdditionalCostToEnterMatching Anywhere $ RevealChaosTokensCost (toSource a) 1]
      whenM (selectAny $ locationWithInvestigator iid <> LocationWithShroud (atMost 2)) do
        modified_ a (AbilityTarget iid (AbilityRef (toSource a) 1)) [ActionCostModifier (-1)]
    _ -> pure ()

instance HasAbilities LostTheTrail where
  getAbilities (LostTheTrail a) = [restricted a 1 InYourThreatArea doubleActionAbility]

instance RunMessage LostTheTrail where
  runMessage msg t@(LostTheTrail attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasCopy <- selectAny $ treacheryIs Cards.lostTheTrail <> treacheryInThreatAreaOf iid
      if hasCopy then gainSurge attrs else placeInThreatArea attrs iid
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      continue_ iid
      when (any ((`elem` goesAstray) . (.face)) tokens) $ doStep 1 msg
      pure t
    DoStep 1 (RequestedChaosTokens (isSource attrs -> True) (Just iid) _) -> do
      intended <- fmap moveDestination <$> field InvestigatorMovement iid
      let notIntended = case intended of
            Just (ToLocation lid) -> not_ (LocationWithId lid)
            _ -> Anywhere
      elsewhere <-
        select $ ConnectedFrom ForMovement (locationWithInvestigator iid) <> notIntended
      -- With nowhere else to go the move cannot be redirected, so it does not
      -- happen at all; the revealed token was still paid.
      if null elsewhere
        then cancelMovement attrs iid
        else chooseOrRunOneM iid $ targets elsewhere $ handleTarget iid attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      replaceMovement iid \m -> m {moveDestination = ToLocation lid}
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LostTheTrail <$> liftRunMessage msg attrs

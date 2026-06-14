module Arkham.Act.Cards.ThePhantomShop (thePhantomShop) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Artifact))

newtype ThePhantomShop = ThePhantomShop ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomShop :: ActCard ThePhantomShop
thePhantomShop = act (1, A) ThePhantomShop Cards.thePhantomShop Nothing

-- | An Artifact asset controlled by an investigator (used both for the objective
-- condition and for counting toward the resolution branch).
controlledArtifact :: InvestigatorMatcher -> AssetMatcher
controlledArtifact who = AssetWithTrait Artifact <> AssetControlledBy who

instance HasAbilities ThePhantomShop where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ You <> at_ (LocationWithCardsUnderneath AnyCards))
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) YourLocation)
    , restricted
        a
        2
        ( exists
            $ controlledArtifact
            $ InvestigatorAt (locationIs Locations.tillinghastEsoterica)
        )
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage ThePhantomShop where
  runMessage msg a@(ThePhantomShop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mLoc <- field InvestigatorLocation iid
      for_ mLoc \loc -> do
        cards <- field LocationCardsUnderneath loc
        for_ (listToMaybe cards) \card ->
          if toCardType card == AssetType
            then do
              -- "if it is an asset, take control of it" -- put it into play under
              -- this investigator's control rather than drawing it to hand.
              obtainCard card
              takeControlOfSetAsideAsset iid card
            else drawCard iid card
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      artifactCount <- selectCount $ controlledArtifact Anyone
      if artifactCount >= 5
        then push R1
        else push R2
      advanceActDeck attrs
      pure a
    _ -> ThePhantomShop <$> liftRunMessage msg attrs

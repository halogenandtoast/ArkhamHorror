module Arkham.Act.Cards.ThePhantomShop (thePhantomShop) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Artifact))

newtype ThePhantomShop = ThePhantomShop ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomShop :: ActCard ThePhantomShop
thePhantomShop = act (1, A) ThePhantomShop Cards.thePhantomShop Nothing

{- | An Artifact asset controlled by an investigator (used both for the objective
condition and for counting toward the resolution branch).
-}
controlledArtifact :: InvestigatorMatcher -> AssetMatcher
controlledArtifact who = AssetWithTrait Artifact <> AssetControlledBy who

instance HasAbilities ThePhantomShop where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (youExist $ at_ (LocationWithCardsUnderneath AnyCards))
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) YourLocation)
    , -- "the investigators may advance", so this one is theirs to decline
      restricted
        a
        2
        ( exists
            $ controlledArtifact
            $ InvestigatorAt (locationIs Locations.tillinghastEsotericaEphemeralShop)
        )
        $ Objective
        $ freeReaction (RoundEnds #when)
    ]

instance RunMessage ThePhantomShop where
  runMessage msg a@(ThePhantomShop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        cards <- field LocationCardsUnderneath lid
        for_ (listToMaybe cards) \card ->
          if toCardType card == AssetType
            then do
              -- "If it is an asset, take control of it": the artifacts go straight
              -- into play rather than being drawn and resolved.
              obtainCard card
              createAssetAt_ card (InPlayArea iid)
            else drawCard iid card
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "If the investigators control 5 or more Artifact assets... Otherwise..."
      -- The "otherwise" branch's 1 bonus experience is awarded with the rest of the
      -- experience in Resolution 2.
      recoveredArtifacts <- (>= 5) <$> selectCount (controlledArtifact Anyone)
      push $ if recoveredArtifacts then R1 else R2
      advanceActDeck attrs
      pure a
    _ -> ThePhantomShop <$> liftRunMessage msg attrs

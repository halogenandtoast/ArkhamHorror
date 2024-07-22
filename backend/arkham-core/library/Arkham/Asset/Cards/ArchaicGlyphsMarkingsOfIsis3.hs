module Arkham.Asset.Cards.ArchaicGlyphsMarkingsOfIsis3 (
  archaicGlyphsMarkingsOfIsis3,
  ArchaicGlyphsMarkingsOfIsis3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype ArchaicGlyphsMarkingsOfIsis3 = ArchaicGlyphsMarkingsOfIsis3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsMarkingsOfIsis3 where
  getAbilities (ArchaicGlyphsMarkingsOfIsis3 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

archaicGlyphsMarkingsOfIsis3 :: AssetCard ArchaicGlyphsMarkingsOfIsis3
archaicGlyphsMarkingsOfIsis3 = asset ArchaicGlyphsMarkingsOfIsis3 Cards.archaicGlyphsMarkingsOfIsis3

instance RunMessage ArchaicGlyphsMarkingsOfIsis3 where
  runMessage msg a@(ArchaicGlyphsMarkingsOfIsis3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (toAbilitySource attrs 1) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid source (isTarget attrs -> True) n -> do
      assets <-
        flip (fieldMap InvestigatorHand) iid
          $ filterBy [(`cardMatch` AssetType), isJust . cdCost . toCardDef, (<= n) . getCost]

      player <- getPlayer iid
      pushAll
        [ chooseUpToN player 1 "Do not play an asset"
            $ [targetLabel (toCardId card) [PutCardIntoPlay iid card Nothing NoPayment []] | card <- assets]
        , Successful (#investigate, toTarget lid) iid source (toTarget lid) n
        ]
      pure a
    _ -> ArchaicGlyphsMarkingsOfIsis3 <$> runMessage msg attrs

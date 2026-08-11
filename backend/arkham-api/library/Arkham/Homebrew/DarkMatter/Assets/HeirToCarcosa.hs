module Arkham.Homebrew.DarkMatter.Assets.HeirToCarcosa (heirToCarcosa) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorMentalTrauma))
import Arkham.Matcher
import Arkham.Message (ShuffleIn (..))
import Arkham.Projection

newtype HeirToCarcosa = HeirToCarcosa AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirToCarcosa :: AssetCard HeirToCarcosa
heirToCarcosa = asset HeirToCarcosa Cards.heirToCarcosa

{- | "When earning experience during the resolution of a scenario, the
investigator with the least mental trauma (choose one if there are more than
one) earns 1 additional experience for each of their mental trauma."

A modifier cannot pose a choice, so ties resolve to the first investigator in
turn order rather than by player choice.
-}
instance HasModifiersFor HeirToCarcosa where
  getModifiersFor (HeirToCarcosa a) = do
    investigators <- select Anyone
    traumas <- for investigators \iid -> (iid,) <$> field InvestigatorMentalTrauma iid
    case sortOn snd traumas of
      ((iid, n) : _) | n > 0 -> modifyEach a [iid] [XPModifier "Heir to Carcosa" n]
      _ -> pure ()

instance RunMessage HeirToCarcosa where
  runMessage msg a@(HeirToCarcosa attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addCampaignCardToDeck iid DoNotShuffleIn Cards.heirToCarcosa
      pure a
    _ -> HeirToCarcosa <$> liftRunMessage msg attrs

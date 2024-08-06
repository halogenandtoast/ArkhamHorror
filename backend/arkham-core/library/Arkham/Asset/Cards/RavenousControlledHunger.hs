module Arkham.Asset.Cards.RavenousControlledHunger (
  ravenousControlledHunger,
  RavenousControlledHunger (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (getController)
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype RavenousControlledHunger = RavenousControlledHunger AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousControlledHunger :: AssetCard RavenousControlledHunger
ravenousControlledHunger = asset RavenousControlledHunger Cards.ravenousControlledHunger

instance HasModifiersFor RavenousControlledHunger where
  getModifiersFor (InvestigatorTarget iid) (RavenousControlledHunger a) | a `controlledBy` iid = do
    n <- fieldMap AssetCardsUnderneath (min 5 . length) a.id
    pure $ toModifiers a [SkillModifier sType n | sType <- [minBound ..]]
  getModifiersFor _ _ = pure []

instance HasAbilities RavenousControlledHunger where
  getAbilities (RavenousControlledHunger a) = [restrictedAbility a 1 (ControlsThis <> criteria) $ ForcedAbility AnyWindow]
   where
    criteria = if length (assetCardsUnderneath a) < 5 then Never else NoRestriction

instance RunMessage RavenousControlledHunger where
  runMessage msg a@(RavenousControlledHunger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      let
        ravenousUncontrolledHunger =
          PlayerCard
            $ lookupPlayerCard Cards.ravenousUncontrolledHunger (toCardId attrs)
        subjectId = getController attrs
      push $ ReplaceInvestigatorAsset subjectId attrs.id ravenousUncontrolledHunger
      pure $ RavenousControlledHunger $ attrs & flippedL .~ True
    _ -> RavenousControlledHunger <$> liftRunMessage msg attrs

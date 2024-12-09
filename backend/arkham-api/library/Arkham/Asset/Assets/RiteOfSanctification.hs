module Arkham.Asset.Assets.RiteOfSanctification (riteOfSanctification, RiteOfSanctification (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RiteOfSanctification = RiteOfSanctification AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSanctification :: AssetCard RiteOfSanctification
riteOfSanctification =
  assetWith RiteOfSanctification Cards.riteOfSanctification (setMeta @Bool False)

instance HasModifiersFor RiteOfSanctification where
  getModifiersFor (RiteOfSanctification a) = do
    modifySelectWhen a a.ready (InvestigatorAt $ locationWithAsset a) [CanReduceCostOf AnyCard 2]

instance HasAbilities RiteOfSanctification where
  getAbilities (RiteOfSanctification attrs) =
    let active = toResult @Bool attrs.meta
     in restricted
          attrs
          1
          ControlsThis
          ( ReactionAbility
              (PlayCard #when (at_ YourLocation) #any)
              (exhaust attrs <> ReleaseChaosTokensCost 1 #any)
          )
          : [restricted attrs 2 (exists $ be attrs <> AssetWithoutSealedTokens) Anytime | active]

getDetails :: [Window] -> (InvestigatorId, Card)
getDetails [] = error "Wrong window"
getDetails ((windowType -> Window.PlayCard iid cardPlay) : _) = (iid, cardPlay.card)
getDetails (_ : xs) = getDetails xs

instance RunMessage RiteOfSanctification where
  runMessage msg a@(RiteOfSanctification attrs) = runQueueT $ case msg of
    ResolvedCard _ card | card.id == toCardId attrs -> do
      RiteOfSanctification <$> liftRunMessage msg (setMeta True attrs)
    UseCardAbility _iid (isSource attrs -> True) 1 (getDetails -> (current, card)) _ -> do
      costModifier attrs current $ ReduceCostOf (CardWithId card.id) 2
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure a
    _ -> RiteOfSanctification <$> liftRunMessage msg attrs

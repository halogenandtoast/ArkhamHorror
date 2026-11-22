module Arkham.Asset.Assets.KnightOfCelephais (knightOfCelephais) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype KnightOfCelephais = KnightOfCelephais AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightOfCelephais :: AssetCard KnightOfCelephais
knightOfCelephais = ally KnightOfCelephais Cards.knightOfCelephais (3, 2)

instance HasModifiersFor KnightOfCelephais where
  getModifiersFor (KnightOfCelephais a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance HasAbilities KnightOfCelephais where
  getAbilities (KnightOfCelephais a) =
    [ restricted a 1 (ControlsThis <> hasADreamToRemove)
        $ freeReaction (AssetDefeated #when ByAny $ be a)
    ]
   where
    -- Without a dream to remove the printed reaction is offered and can do
    -- nothing (#6017).
    hasADreamToRemove = exists (DreamingCardControlledBy You <> DreamingCardWithDreams (atLeast 1))

instance RunMessage KnightOfCelephais where
  runMessage msg a@(KnightOfCelephais attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      dreaming <-
        select $ DreamingCardControlledBy (InvestigatorWithId iid) <> DreamingCardWithDreams (atLeast 1)
      chooseTargetM iid dreaming \dc -> removeTokens (attrs.ability 1) dc Dream 1
      pure a
    _ -> KnightOfCelephais <$> liftRunMessage msg attrs

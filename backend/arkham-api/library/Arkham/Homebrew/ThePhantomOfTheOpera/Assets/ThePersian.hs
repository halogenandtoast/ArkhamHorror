module Arkham.Homebrew.ThePhantomOfTheOpera.Assets.ThePersian (thePersian) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Effect.Window (EffectWindow (EffectNextCardPlayedWindow))
import Arkham.Helpers.Location (getAccessibleLocations, getLocationOf, withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype ThePersian = ThePersian AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePersian :: AssetCard ThePersian
thePersian = ally ThePersian Cards.thePersian (2, 2)

instance HasModifiersFor ThePersian where
  getModifiersFor (ThePersian a) =
    controllerGets a [SkillModifier #intellect 1, SkillModifier #combat 1]

usedLocations :: AssetAttrs -> [LocationId]
usedLocations a = toResultDefault [] a.meta

instance HasAbilities ThePersian where
  getAbilities (ThePersian a) =
    [ controlled a 1 (notExists $ YourLocation <> mapOneOf LocationWithId (usedLocations a))
        $ triggered (LastClueRemovedFromLocation #after YourLocation) (exhaust a)
    , controlled a 2 canResolveEitherHalf
        $ triggered (CampaignEvent #after (Just You) "exposed[decoy]") (exhaust a)
    ]
   where
    canResolveEitherHalf =
      oneOf
        [ youExist (InvestigatorCanMoveTo (a.ability 2) AccessibleLocation)
        , exists (connectedFrom YourLocation <> RevealedLocation <> LocationWithAnyClues)
        ]

instance RunMessage ThePersian where
  runMessage msg a@(ThePersian attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      effectWithSource (attrs.ability 1) iid do
        apply $ ReduceCostOf #event 2
        during $ EffectNextCardPlayedWindow iid #event
        removeOn #round
      mlid <- getLocationOf iid
      pure . ThePersian $ maybe attrs (\lid -> setMeta (lid : usedLocations attrs) attrs) mlid
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      connected <- getAccessibleLocations iid source
      withLocationOf iid \lid -> do
        withClues <-
          select $ connectedFrom (locationWithInvestigator iid) <> RevealedLocation <> LocationWithAnyClues
        scenarioI18n $ scope "thePersian" $ chooseOneM iid do
          unless (null connected) do
            unscoped $ labeled "moveToConnecting" $ chooseTargetM iid connected $ moveTo source iid
          unless (null withClues) do
            labeled "moveClueFromConnecting" do
              chooseTargetM iid withClues \from -> moveTokens source from lid #clue 1
      pure a
    _ -> ThePersian <$> liftRunMessage msg attrs

{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.JimsTrumpet where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Target
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Token
import Arkham.Types.Window
import ClassyPrelude
import Safe (fromJustNote)

newtype JimsTrumpet = JimsTrumpet Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

jimsTrumpet :: AssetId -> JimsTrumpet
jimsTrumpet uuid =
  JimsTrumpet $ (baseAttrs uuid "02012") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator JimsTrumpet where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator JimsTrumpet where
  getActions i (WhenRevealToken who Skull) (JimsTrumpet Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      let
        ability = mkAbility
          (AssetSource assetId)
          1
          (ReactionAbility (WhenDrawToken who Skull))
        ownerId = fromJustNote "must be owned" assetInvestigator
      locationId <- asks (getId ownerId)
      connectedLocationIds <- map unConnectedLocationId . setToList <$> asks
        (getSet locationId)
      investigatorIds <- for (locationId : connectedLocationIds)
        $ \lid -> setToList <$> asks (getSet @InvestigatorId lid)
      horrorCounts <- for (concat investigatorIds) $ \targetId -> do
        unHorrorCount <$> asks (getCount targetId)
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction ownerId ability
        | (ownerId, ability) `notElem` usedAbilities && any (> 0) horrorCounts
        ]
  getActions i window (JimsTrumpet x) = getActions i window x

instance (AssetRunner env) => RunMessage env JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs@Attrs {..}) = case msg of
    UseCardAbility _ (AssetSource aid) _ 1 | aid == assetId -> do
      let ownerId = fromJustNote "must be owned" assetInvestigator
      locationId <- asks (getId ownerId)
      connectedLocationIds <- map unConnectedLocationId . setToList <$> asks
        (getSet locationId)
      investigatorIds <- concat <$> for
        (locationId : connectedLocationIds)
        (\lid -> setToList <$> asks (getSet @InvestigatorId lid))
      pairings <- for investigatorIds $ \targetId ->
        (targetId, ) . unHorrorCount <$> asks (getCount targetId)
      let choices = map fst $ filter ((> 0) . snd) pairings
      a <$ unshiftMessage
        (Ask
          ownerId
          (ChooseOne [ HealHorror (InvestigatorTarget iid) 1 | iid <- choices ])
        )
    _ -> JimsTrumpet <$> runMessage msg attrs

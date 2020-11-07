{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DrMilanChristopher where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype DrMilanChristopher = DrMilanChristopher Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drMilanChristopher :: AssetId -> DrMilanChristopher
drMilanChristopher uuid = DrMilanChristopher $ baseAttrs uuid "01033" $ do
  slots .= [AllySlot]
  health ?= 1
  sanity ?= 2

instance HasModifiersFor env DrMilanChristopher where
  getModifiersFor _ (InvestigatorTarget iid) (DrMilanChristopher a) =
    pure [ SkillModifier SkillIntellect 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env DrMilanChristopher where
  getActions i window (DrMilanChristopher x) = getActions i window x

instance AssetRunner env => RunMessage env DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs) = case msg of
    SuccessfulInvestigation iid _ | getInvestigator attrs == iid ->
      a <$ unshiftMessage
        (chooseOne
          iid
          [ UseCardAbility iid (toSource attrs) Nothing 1
          , Continue "Do not use Dr. Christopher Milan's ability"
          ]
        )
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> DrMilanChristopher <$> runMessage msg attrs

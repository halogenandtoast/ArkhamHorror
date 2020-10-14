{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Graveyard where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude

newtype Graveyard = Graveyard Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graveyard :: Graveyard
graveyard =
  Graveyard
    $ (baseAttrs "01133" "Graveyard" 1 (PerPlayer 2) Hourglass [Circle] [Arkham]
      )
        { locationVictory = Just 1
        }

instance HasModifiersFor env investigator Graveyard where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Graveyard where
  getActions i window (Graveyard attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Graveyard where
  runMessage msg (Graveyard attrs@Attrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (LocationSource lid)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
          []
          [ Ask
              iid
              (ChooseOne
                [ InvestigatorAssignDamage iid (LocationSource lid) 0 2
                , MoveTo iid "01125"
                ]
              )
          ]
          []
          mempty
        )
      Graveyard <$> runMessage msg attrs
    _ -> Graveyard <$> runMessage msg attrs

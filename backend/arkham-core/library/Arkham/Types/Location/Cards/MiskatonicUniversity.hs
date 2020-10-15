{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MiskatonicUniversity where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicUniversity = MiskatonicUniversity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicUniversity :: MiskatonicUniversity
miskatonicUniversity = MiskatonicUniversity $ (baseAttrs
                                                "01129"
                                                "Miskatonic University"
                                                4
                                                (PerPlayer 2)
                                                Diamond
                                                [T, Plus, Circle, Square]
                                                [Arkham]
                                              )
  { locationVictory = Just 1
  }

instance HasModifiersFor env investigator MiskatonicUniversity where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator MiskatonicUniversity where
  getActions i NonFast (MiskatonicUniversity attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions i NonFast attrs
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               (getId () i)
               (mkAbility (LocationSource "01129") 1 (ActionAbility 1 Nothing))
           | atLocation i attrs && hasActionsRemaining i Nothing locationTraits
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (SearchTopOfDeck
        iid
        (InvestigatorTarget iid)
        6
        [Tome, Spell]
        ShuffleBackIn
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs

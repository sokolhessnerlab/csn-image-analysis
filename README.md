# Analysis of CSN Image Categorization

Uses `shlab.imgct` tools and interacts with data via the mounted shared drive
directory `imgct`.

## Data

Image categorization task data should live under a single root directory.
Within, six main subdirectories with relevant data types will exist.

```bash
.
├── blocks
│   ├── block_01.txt
│   .   .
│   .   .
│   .   .
│   └── block_18.txt
├── clean
│   ├── clean_01.tsv
│   .   .
│   .   .
│   .   .
│   └── clean_18.tsv
├── imports
│   ├── iaps_emotion_ratings.xlsx
│   ├── naps_be_emotion_ratings.xlsx
│   ├── naps_ero_emotion_ratings.xlsx
│   └── oasis_emotion_ratings.xlsx
├── keys
│   ├── categorization_key.txt
│   └── validation_key.txt
├── raw
│   ├── erotic_image_experimenter_ratings.csv
│   └── qualtrics.tsv
└── results
    ├── all_participant_validations.tsv
    ├── categorized_0_valid.tsv
    .   .
    .   . 
    .   .
    ├── categorized_5_valid.tsv
    ├── general_emotion_ratings.tsv
    └── master_table.tsv
```

## Notebooks

This list of notebooks outlines the analytical procedure used to evaluate CSN
images.

### Preparing Data

- [Clean Qualtrics Export](./clean_qualtrics_export.md)
- [Validate Response Data](./validate_response_data.md)

### Analyzing Data

- [Validations Analysis](./validations.md)
- [Image Categorizations Analysis](./categorizations.md)
- [Emotion Ratings Analysis](./emotion_ratings.md)

### Master Results Data

- [Master Table](./master_table.md)
